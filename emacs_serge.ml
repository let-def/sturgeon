open Emacs_sexp

type 'a result =
  | Feed of 'a
  | Quit of basic

type 'a neg = 'a result -> unit

type dual =
  | Once of t neg
  | Sink of t neg

and t = dual sexp

let gensym () =
  let counter = ref 0 in
  fun () -> incr counter; !counter

let cancel_message = Quit (S "cancel")
let finalize_message = Quit (S "finalize")

let add_finalizer (dual : t neg) =
  let finalize f = f finalize_message in
  Gc.finalise finalize dual

type endpoint = {
  stdout: basic -> unit;
  query: t -> unit;
}

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

let connect ?(stderr : ('a error -> unit) option) f_endpoint : endpoint =

  let endpoint = ref {
      query = (fun _ -> invalid_arg "Query before initialization");
      stdout = (fun _ -> invalid_arg "Output before initialization");
    }
  in

  let eof = ref false in

  let gensym = gensym () in

  let table : (int, dual) Hashtbl.t = Hashtbl.create 7 in

  (* Lower: turn closures into ground sexp *)

  let lower (t : t) : basic =
    let map : basic -> basic = function
      | C (S "meta", x) -> C (S "meta", C (S "escape", x))
      | x -> x
    and inj (dual : dual) : basic =
      let addr = gensym () in
      Hashtbl.add table addr dual;
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
          if !eof then
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
              (!endpoint).stdout (C (S "feed", C (addr, lower x)))
            | Quit x ->
              closed := true;
              (!endpoint).stdout (C (S "quit", C (addr, x)))
        in
        add_finalizer dual;
        M (if is_once then Once dual else Sink dual)
      | x -> x
    and inj : void -> t = void
    in
    transform_cons ~inj ~map t
  in

  let get_addr = function
    | I addr -> addr, Hashtbl.find table addr
    | _ -> raise Not_found
  in

  let t = {
    stdout = begin
      if !eof then ignore
      else function
        | C (S "query", payload) ->
          (!endpoint).query (upper payload)
        | C (S "feed", C (addr, payload)) as msg ->
          let x = upper payload in
          begin match get_addr addr with
            | addr, Once t ->
              Hashtbl.remove table addr;
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
              Hashtbl.remove table addr;
              t (Quit x)
            | exception Not_found ->
              begin match stderr with
                | Some f -> f (`Quit_unknown msg)
                | None -> ()
              end
          end
        | S "end" ->
          eof := true;
          let exns = ref [] in
          Hashtbl.iter (fun _ (Sink t | Once t) ->
              try t cancel_message
              with exn -> exns := exn :: !exns
            ) table;
          Hashtbl.reset table;
          if !exns <> [] then
            begin match stderr with
              | Some f -> f (`Exceptions_during_shutdown !exns)
              | None -> ()
            end
        | cmd ->
          begin match stderr with
            | Some f -> f (`Invalid_command cmd)
            | None -> ()
          end
    end;

    query = begin fun t ->
      if !eof then (
        cancel ?stderr t;
        match stderr with
        | None -> ()
        | Some f -> f (`Query_after_eof t)
      ) else
        (!endpoint).stdout (C (S "query", lower t))
    end

  } in

  endpoint := f_endpoint ~remote_query:t.query;
  t

let close endpoint = endpoint.stdout (S "end")
