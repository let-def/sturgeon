open Sturgeon_sexp

type 'a result =
  | Feed of 'a
  | Quit of basic

type 'a neg = 'a result -> unit

type dual =
  | Once of t neg
  | Sink of t neg

and t = dual sexp

let cancel_message = Quit (S "cancel")
let finalize_message = Quit (S "finalize")

let add_finalizer (dual : t neg) addr =
  let finalize _addr = dual finalize_message in
  Gc.finalise finalize addr

(* Cancel : abort computation, release ressources  *)
let lower_cancel ?stderr (t : t) : basic =
  let exns = ref [] in
  let map : basic -> basic = function
    | C (S "meta", x) -> C (S "meta", C (S "escape", x))
    | x -> x
  and inj (dual : dual) : basic =
    begin
      let (Once t | Sink t) = dual in
      try t cancel_message
      with exn -> exns := exn :: !exns
    end;
    match dual with
    | Once _ -> C (S "meta", C (S "once", S "cancelled"))
    | Sink _ -> C (S "meta", C (S "sink", S "cancelled"))
  in
  let result = transform_cons ~inj ~map t in
  if !exns <> [] then
    (match stderr with
     | Some f -> f (`Exceptions_during_cancellation (t, !exns))
     | None -> ());
  result

let cancel ?stderr (t : t) =
  let exns = ref [] in
  let rec aux = function
    | C (a, b) -> aux a; aux b
    | P t -> aux t
    | S _ | T _ -> ()
    | I _ | F _ -> ()
    | V xs -> List.iter aux xs
    | M (Once t | Sink t) ->
      try t cancel_message
      with exn -> exns := exn :: !exns
  in
  let result = aux t in
  if !exns <> [] then
    (match stderr with
     | Some f -> f (`Exceptions_during_cancellation (t, !exns))
     | None -> ());
  result

type 'a error =
  [ `Already_closed  of t result
  | `Query_after_eof of t
  | `Invalid_command of basic
  | `Feed_unknown    of basic
  | `Quit_unknown    of basic
  | `Exceptions_during_cancellation of t * exn list
  | `Exceptions_during_shutdown of exn list
  ]

type status = {
  mutable state: [`Greetings | `Main | `Closed];
  mutable gensym: int;
  table: (int, dual) Hashtbl.t;
}

type output = basic -> unit

let gensym status =
  status.gensym <- status.gensym + 1;
  status.gensym

let connect
    ?(greetings=sym_nil) ?cogreetings
    ?(stderr : ('a error -> unit) option)
    stdout
  : output * status
  =

  let status = {
    state = `Greetings;
    gensym = 0;
    table = Hashtbl.create 7;
  } in

  (* Lower: turn closures into ground sexp *)

  let lower (t : t) : basic =
    let map : basic -> basic = function
      | C (S "meta", x) -> C (S "meta", C (S "escape", x))
      | x -> x
    and inj (dual : dual) : basic =
      let addr = gensym status in
      Hashtbl.add status.table addr dual;
      let sym = match dual with
        | Once _ -> S "once"
        | Sink _ -> S "sink"
      in
      C (S "meta", C (sym, I addr))
    in
    transform_cons ~inj ~map t
  in

  (* Upper: inject closures into ground sexp *)

  let upper (t : basic) : t =
    let map : t -> t = function
      | C (S "meta", C (S "escape", x)) ->
        C (S "meta", x)
      | C (S "meta", C (S ("once" | "sink" as kind), addr)) ->
        let addr = lower_cancel ?stderr addr in
        let is_once = kind = "once" in
        let closed = ref false in
        let dual msg =
          if status.state = `Closed then
            match msg with
            | Feed x -> cancel ?stderr x
            | Quit _ -> ()
          else if !closed then
            if msg == finalize_message then ()
            else begin
              begin match msg with
                | Feed x -> cancel ?stderr x
                | Quit _ -> ()
              end;
              match stderr with
              | Some f -> f (`Already_closed msg)
              | None -> ()
            end
          else match msg with
            | Feed x ->
              closed := is_once;
              stdout (C (S "feed", C (addr, lower x)))
            | Quit x ->
              closed := true;
              stdout (C (S "quit", C (addr, x)))
        in
        add_finalizer dual addr;
        M (if is_once then Once dual else Sink dual)
      | x -> x
    and inj : void -> t = void
    in
    transform_cons ~inj ~map t
  in

  let get_addr = function
    | I addr -> addr, Hashtbl.find status.table addr
    | _ -> raise Not_found
  in

  let remote (cmd : basic) =
    match status.state with
    | `Closed -> cancel ?stderr (upper cmd)
    | `Greetings ->
      begin match cmd with
        | C (S "greetings", C (I 1, payload)) ->
          status.state <- `Main;
          begin match cogreetings with
            | Some f -> f (upper payload)
            | None -> cancel ?stderr (upper payload)
          end
        | _ -> cancel ?stderr (upper cmd)
      end
    | `Main -> match cmd with
      | C (S "feed", C (addr, payload)) as msg ->
        let x = upper payload in
        begin match get_addr addr with
          | addr, Once t ->
            Hashtbl.remove status.table addr;
            t (Feed x)
          | _, Sink t -> t (Feed x)
          | exception Not_found ->
            cancel ?stderr x;
            begin match stderr with
              | Some f -> f (`Feed_unknown msg)
              | None -> ()
            end
        end

      | C (S "quit", C (addr, x)) as msg ->
        begin match get_addr addr with
          | addr, (Once t | Sink t) ->
            Hashtbl.remove status.table addr;
            t (Quit x)
          | exception Not_found ->
            begin match stderr with
              | Some f -> f (`Quit_unknown msg)
              | None -> ()
            end
        end

      | S "end" ->
        status.state <- `Closed;
        let exns = ref [] in
        Hashtbl.iter (fun _ (Sink t | Once t) ->
            try t cancel_message
            with exn -> exns := exn :: !exns
          ) status.table;
        Hashtbl.reset status.table;
        begin try stdout (S "end")
          with exn -> exns := exn :: !exns
        end;
        if !exns <> [] then
          begin match stderr with
            | Some f -> f (`Exceptions_during_shutdown !exns)
            | None -> ()
          end

      | cmd ->
        cancel ?stderr (upper cmd);
        begin match stderr with
          | Some f ->
            f (`Invalid_command cmd)
          | None -> ()
        end
  in
  stdout (lower (C (S "greetings", C (I 1, greetings))));
  remote, status

let close remote = remote (S "end")

let pending_sessions status =
  Hashtbl.length status.table

let is_closed status = status.state = `Closed
