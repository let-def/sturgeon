open Emacs_sexp
type 'a sexp = 'a t
type basic = Emacs_sexp_basic.void sexp

exception Aborted of basic

type 'a result =
  | Feed  of 'a
  | Close
  | Abort of basic

type 'a neg = 'a result -> unit

type dual =
  | Once of t neg
  | Sink of t neg

and t = dual sexp

let gensym () =
  let counter = ref 0 in
  fun () -> incr counter; !counter

type address = basic

module Cmd = struct
  type t =
    | Query of basic
    | Dispatch of address * basic
    | Abort of address * basic
    | Close of address
end

let close_message = S "end"

let connect (writer : basic -> unit) : (basic -> unit) * (t -> unit) =
  let gensym = gensym () in

  let table : (int, dual) Hashtbl.t = Hashtbl.create 7 in

  let lower = function
    | C (S "meta", x) -> C (S "meta", C (S "escape", x))
    | M dual ->
      let addr = gensym () in
      Hashtbl.add table addr dual;
      let sym = match dual with
        | Once _ -> S "once"
        | Sink _ -> S "sink"
      in
      C (S "meta", C (sym, I addr))
    | x -> x
  in
  let up_once addr = function
    |
  let upper = function
    | C (S "meta", C (S "escape", x)) ->
      C (S "meta", x)
    | C (S "meta", C (S "once", addr)) ->
      M (
    | C (S "meta", C (S "sink", addr)) ->

  let read_command = function
    | C (S "query"    , payload) -> Cmd.Query payload
    | C (S "dispatch" , C (addr, payload)) -> Cmd.Dispatch (addr, payload)
    | C (S "close"    , addr) -> Cmd.Close addr
    | _ -> invalid_arg "Unhandled command"
  in
  let write_command = function
    | Cmd.Query payload -> C (S "query", payload)
    | Cmd.Dispatch (addr, payload) -> C (S "dispatch" , C (addr, payload))
    | Cmd.Abort (addr, payload) -> C (S "abord", C (addr, payload))
    | Cmd.Close addr -> C (S "close", addr)
  in
