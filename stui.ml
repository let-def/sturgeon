open Sexp
open Session

type flag = [`Clickable | `Editable | `Clicked]
type patch = flag Inuit.patch

module Remote_pipe = struct
  type revision = {
    r_remote: int;
    r_local: int;
  }

  type command =
    | Patch of patch
    | Ack

  type command_stream = {
    mutable sink: t neg option;
    mutable closed: bool;
    mutable queue: (revision * command) list;
  }

  let sexp_of_revision {r_remote; r_local} =
    C (I r_remote, I r_local)

  let sexp_of_flags flags =
    let rec aux acc = function
      | [] -> acc
      | `Clickable :: xs -> aux (C (S "clickable", acc)) xs
      | `Editable  :: xs -> aux (C (S "editable", acc)) xs
      | `Clicked   :: xs -> aux (C (S "clicked", acc)) xs
    in
    aux (S "nil") flags

  let flags_of_sexp sexp =
    let rec aux acc = function
      | C (S "clickable", xs) -> aux (`Clickable :: acc) xs
      | C (S "editable", xs)  -> aux (`Editable :: acc) xs
      | C (S "clicked", xs)   -> aux (`Clicked :: acc) xs
      | S "nil" -> acc
      | sexp ->
        let sexp =
          Sexp.transform_cons ~inj:(fun _ -> S "<abstract>") ~map:(fun x -> x)
            sexp
        in
        prerr_endline ("Unknown flags: " ^ Sexp.to_string sexp);
        acc
    in
    aux [] sexp

  let sexp_of_command rev = function
    | Patch {Inuit.Patch. offset; old_len; new_len; text; flags} ->
      C (S "patch",
         C (sexp_of_revision rev,
            C (C (I offset, I old_len),
               C (C (I new_len, T text),
                  C (sexp_of_flags flags, sym_nil)))))
    | Ack -> C (S "ack", C (sexp_of_revision rev, sym_nil))

  let cons_if cond x xs = if cond then x :: xs else xs

  let send_command sink (rev,command) =
      sink (Feed (sexp_of_command rev command))

  let replace_sink commands sink =
    begin match commands.sink with
      | Some sink' -> sink' (Quit sym_nil)
      | None -> ()
    end;
    commands.sink <- None;
    match sink with
    | Some f ->
      let rec aux = function
        | [] -> ()
        | ls ->
          commands.queue <- [];
          List.iter (send_command f) (List.rev ls);
          aux commands.queue
      in
      aux commands.queue;
      commands.sink <- sink
    | None -> ()

  type op =
    | R of int * int
    | I of int * int

  type t = {
    commands: command_stream;

    mutable remote: int;
    mutable latest_remote: int;

    mutable local: int;
    mutable revisions: (int * op) list;
    mutable rev_tail: (int * op) list;
  }

  let revision_of_textbuf { remote; local } =
    { r_local = local; r_remote = remote }

  let push_command t command =
    let command = (revision_of_textbuf t, command) in
    if t.commands.closed then ()
    else match t.commands.sink with
      | None -> t.commands.queue <- command :: t.commands.queue
      | Some sink ->
        t.latest_remote <- t.remote;
        send_command sink command

  let update_revisions t (local, remote) =
    t.remote <- remote;
    let rec filter = function
      | (local', _) :: xs when local' <= local -> filter xs
      | xs -> xs
    in
    let rec rev_filter acc = function
      | (local', _) as x :: xs when local' > local ->
        rev_filter (x :: acc) xs
      | _ -> acc
    in
    begin match filter t.rev_tail with
      | [] ->
        t.rev_tail <- rev_filter [] t.revisions;
        t.revisions <- []
      | xs ->
        t.rev_tail <- xs
    end;
    if t.latest_remote < remote - 16 then
      push_command t Ack

  let commute_remove (_,op') (s2, l2) = match op' with
    | R (s1, l1) ->
      let remap x =
        if x < s1 then
          x
        else if x > s1 + l1 then
          x - l1
        else s1
      in
      let e2 = s2 + l2 in
      let s2 = remap s2 and e2 = remap e2 in
      if e2 = s2 then
        raise Not_found
      else (s2, e2 - s2)
    | I (s1, l1) ->
      if s1 < s2 then
        (s2 + l1, l2)
      else if s1 >= s2 + l2 then
        (s2, l2)
      else
        (s2, l2 + l1)

  let commute_point (_,op') s2 = match op' with
    | R (s1, l1) ->
      if s2 < s1 then
        s2
      else if s2 >= s1 + l1 then
        (s2 - l1)
      else
        raise Not_found
    | I (s1, l1) ->
      if s1 <= s2 then
        (s2 + l1)
      else
        s2

  let commute_remote_op t op_kind op_arg =
    let flip f x y = f y x in
    let op_arg = List.fold_left (flip op_kind) op_arg t.rev_tail in
    let op_arg = List.fold_right op_kind t.revisions op_arg in
    op_arg

  let local_change t patch =
    let open Inuit.Patch in
    t.local <- t.local + 1;
    if patch.old_len <> 0 then
      t.revisions <- (t.local, R (patch.offset, patch.old_len)) :: t.revisions;
    if patch.new_len <> 0 then
      t.revisions <- (t.local, I (patch.offset, patch.new_len)) :: t.revisions;
    push_command t (Patch patch)

  let create () =
    let commands = {
      sink   = None;
      closed = false;
      queue  = [];
    } in
    let t = { commands; revisions = []; rev_tail = [];
              remote = 0; latest_remote = 0; local = 0 } in
    let pipe = Inuit.Pipe.make ~change:(local_change t) in
    let handler = M (Sink (function
        | Feed (C (S "sink", M (Sink sink))) ->
          (*Printf.eprintf "GOT SINK!\n";*)
          replace_sink commands (Some sink)

        | Feed (C (S "ack", C (C (I local, I remote), S "nil"))) ->
          update_revisions t (local, remote)

        | Feed (C (S "patch",
                   C (C (I local, I remote),
                      C (C (I offset, I old_len),
                         C (C (I new_len, T text),
                            C (flags, S "nil")))))) ->
          update_revisions t (local, remote);
          begin match
              if old_len <> 0 then
                commute_remote_op t commute_remove (offset, old_len)
              else
                commute_remote_op t commute_point offset, 0
            with
            | exception Not_found -> ()
            | (offset, replace) ->
              let flags = flags_of_sexp flags in
              (* Check sexp new_len = patch.new_len *)
              Inuit.Pipe.commit pipe
                (Inuit.Patch.make ~offset ~replace flags text)
          end
        | Feed r ->
          cancel r
        | Quit (S "close") ->
          replace_sink commands None;
          commands.closed <- true;
          ()
        | Quit _ -> ()
      ))
    in
    handler, pipe
end

type shell_status =
  | Pending of Session.t list
  | Connected of Session.t Session.neg

type buffer_shell = name:string -> flag Inuit.pipe -> unit

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
    let session, pb = Remote_pipe.create () in
    Inuit.Pipe.connect ~a:pa ~b:pb;
    let session = sexp_of_list [S "create-buffer"; T name; session] in
    match !status with
    | Pending xs -> status := Pending (session :: xs)
    | Connected t -> t (Feed session)
  in
  sexp, create_buffer

let accept_buffer session pa = match session with
  | M (Sink t) ->
    let session, pb = Remote_pipe.create () in
    Inuit.Pipe.connect ~a:pa ~b:pb;
    t (Feed (C (S "accept", session)))
  | _ -> invalid_arg "Stui.accept_buffer"

let accept_cursor session =
  let cursor, pipe = Inuit.make () in
  accept_buffer session pipe;
  cursor

let create_buffer (shell : buffer_shell) = shell
let create_cursor (shell : buffer_shell) ~name =
  let cursor, pipe = Inuit.make () in
  create_buffer shell ~name pipe;
  cursor
