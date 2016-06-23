open Sturgeon_sexp
open Sturgeon_session
open Inuit

type flag = [`Clickable | `Clicked | `Editable | `Invisible]

let dump_sexp sexp =
  let inj _ = S "<abstract>" and map x = x in
  to_string (transform_cons ~inj ~map sexp)

let sexp_of_revision {Remote. remote; local} =
  C (I remote, I local)

let revision_of_sexp = function
  | C (I remote, I local) -> {Remote. remote; local}
  | sexp -> failwith ("revision_of_sexp: cannot parse " ^ dump_sexp sexp)

let sexp_of_flags flags =
  let rec aux acc = function
    | [] -> acc
    | `Clickable :: xs -> aux (C (S "clickable", acc)) xs
    | `Editable  :: xs -> aux (C (S "editable", acc)) xs
    | `Invisible :: xs -> aux (C (S "invisible", acc)) xs
    | `Clicked   :: xs -> aux (C (S "clicked", acc)) xs
  in
  aux (S "nil") flags

let flags_of_sexp sexp =
  let rec aux acc = function
    | C (S "clickable", xs) -> aux (`Clickable :: acc) xs
    | C (S "clicked", xs)   -> aux (`Clicked :: acc) xs
    | C (S "editable", xs)  -> aux (`Editable :: acc) xs
    | C (S "invisible", xs) -> aux (`Invisible :: acc) xs
    | S "nil" -> acc
    | sexp -> prerr_endline ("Unknown flags: " ^ dump_sexp sexp); acc
  in
  aux [] sexp

let sexp_of_remote_patch = function
  | Remote.Patch (rev, {Patch. offset; old_len; new_len; text; flags}) ->
    let region = C (I offset, I old_len) and new_txt = C (I new_len, T text) in
    let patch = C (region, C (new_txt, C (sexp_of_flags flags, sym_nil))) in
    C (S "patch", C (sexp_of_revision rev, patch))
  | Remote.Ack rev ->
    C (S "ack", C (sexp_of_revision rev, sym_nil))

let remote_patch_of_sexp = function
  | C (S "ack", C (revision, S "nil")) ->
    Remote.Ack (revision_of_sexp revision)
  | C (S "patch", C (revision, C (C (I offset, I replace),
                                  C (C (I new_len, T text),
                                     C (flags, S "nil"))))) ->
    let patch = Patch.make ~offset ~replace (flags_of_sexp flags) text in
    Remote.Patch ((revision_of_sexp revision), patch)
  | sexp -> failwith ("remote_patch_of_sexp: cannot parse " ^ dump_sexp sexp)

let remote_patch_socket () =
  let queue = ref [] in
  let socket = Socket.make ~receive:(fun msg -> queue := msg :: !queue) in
  let handler = function
    | Feed (C (S "sink", M (Sink sink))) ->
      let rec aux = function
        | x :: xs ->
          sink (Feed (sexp_of_remote_patch x)); aux xs
        | [] ->
          let xs = !queue in
          queue := [];
          if xs <> [] then aux (List.rev xs)
      in
      aux [];
      Socket.set_receive socket
        (fun msg -> sink (Feed (sexp_of_remote_patch msg)))
    | Feed sexp ->
      Socket.send socket (remote_patch_of_sexp sexp)
    | Quit _ ->
      Socket.close socket
  in
  (Socket.endpoint socket, M (Sink handler))

type buffer_shell = {
  create_buffer : name:string -> flag patch socket -> unit;
}

type shell_status =
  | Pending of Sturgeon_session.t list
  | Connected of Sturgeon_session.t neg

let buffer_greetings () =
  let status = ref (Pending []) in
  let shell = function
    | Feed (M (Sink t)) ->
      begin match !status with
        | Connected _ ->
          failwith "Stui.buffer_greetings: invalid session, already connected"
        | Pending xs ->
          status := Connected t;
          List.iter (fun x -> t (Feed x)) (List.rev xs)
      end
    | _ -> failwith "Stui.buffer_greetings: invalid session, unknown command"
  in
  let sexp = sexp_of_list [S "buffer-shell"; M (Once shell)] in
  let create_buffer ~name pa =
    let remote, local = Remote.make () in
    let session_socket, session_sexp = remote_patch_socket () in
    Socket.connect ~a:remote ~b:session_socket;
    Socket.connect ~a:pa ~b:local;
    let answer = sexp_of_list [S "create-buffer"; T name; session_sexp] in
    match !status with
    | Pending xs -> status := Pending (answer :: xs)
    | Connected t -> t (Feed answer)
  in
  (sexp, {create_buffer})

let accept_buffer session pa = match session with
  | M (Sink t) ->
    let remote, local = Remote.make () in
    let session_socket, session_sexp = remote_patch_socket () in
    Socket.connect ~a:remote ~b:session_socket;
    Socket.connect ~a:pa ~b:local;
    t (Feed (C (S "accept", session)))
  | _ -> invalid_arg "Stui.accept_buffer"

let accept_cursor session =
  let cursor, pipe = Inuit.Cursor.make () in
  accept_buffer session pipe;
  cursor

let create_buffer shell = shell.create_buffer

let create_cursor shell ~name =
  let cursor, pipe = Inuit.Cursor.make () in
  create_buffer shell ~name pipe;
  cursor
