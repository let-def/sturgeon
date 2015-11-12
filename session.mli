open Sexp

type 'a result =
  | Feed  of 'a
  | Quit of basic

type 'a neg = 'a result -> unit

val cancel_message : 'a result
val finalize_message : 'a result

type dual =
  | Once of t neg
  | Sink of t neg

and t = dual sexp

val cancel :
  ?stderr:([> `Exceptions_during_cancellation of t * exn list] -> unit) ->
  t -> unit

type 'a error =
  [ `Already_closed  of t result
  | `Query_after_eof of t
  | `Invalid_command of basic
  | `Feed_unknown    of basic
  | `Quit_unknown    of basic
  | `Exceptions_during_cancellation of t * exn list
  | `Exceptions_during_shutdown of exn list
  ]

type output = basic -> unit
type status

val connect :
  ?greetings:t -> ?cogreetings:(t -> unit) ->
  ?stderr:(_ error -> unit) -> output -> output * status

val close : output -> unit

val pending_sessions : status -> int
val is_closed : status -> bool
