open Emacs_sexp

type 'a result =
  | Feed  of 'a
  | Close
  | Abort of basic

type 'a neg = 'a result -> unit

type dual =
  | Once of t neg
  | Sink of t neg

and t = dual sexp

exception Invalid_message of basic

type endpoint = { stdout : basic -> unit; query : t -> unit; }

val connect : ?stderr:(exn:exn -> string -> unit) -> endpoint -> endpoint
val close : endpoint -> unit

val stderr : exn:exn -> string -> unit
val cancel : ?stderr:(exn:exn -> string -> unit) -> t -> unit

val close_message : 'a sexp
val cancel_message : 'a result
val finalize_message : 'a result
