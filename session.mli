(* {{{ COPYING *(

  This file is part of Sturgeon, a toolkit for remote higher-order control
  flow.

  Copyright (C) 2016  Frédéric Bour  <frederic.bour(_)lakaban.net>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Sexp

(** Session extends S-exp with abstract objects.
    One can now put function-like values inside s-expressions.

    When serializing, the library will take care of representing these values
    by handles that can be used from the remote side.
*)

(** The actions that can be sent to abstract objects. *)
type 'a result =
  | Feed of 'a
  (** Feed is the action to transmit a value to an abstract object.
      See [dual] type below.
      A promise ([Once]) can be fed only once, a stream ([Sink]) can be fed
      an arbitrary number of times. *)
  | Quit of basic
  (** Quit release an abstract object.
      If it is a promise ([Once]), it will be canceled (waiter will receive an
      error), if it is a stream ([Sink]) it will be closed.  *)

(** Negation: consuming values.
    Normal values are [positive]. When you have one of them you own an instance
    of some type.
    Negations are the opposite. When you have one of them all you can do is
    provide an instance of some type.

    Positive and negative values are the building blocks of more complex
    control flows.
    For instance, a function from ['a -> 'b] be encoded as [('a * 'b neg) neg]:
    if you give a ['a], you will own a ['b].
*)
type 'a neg = 'a result -> unit

type dual =
  | Once of t neg (** [Once] is the constructor for linear negations:
                      they can consume only one value. *)
  | Sink of t neg (** [Sink] is the constructor for streams:
                      they can consume arbitrarily many values. *)

(** Finally the type of sessions: it is the S-exp extended with dual values. *)
and t = dual sexp

(** The message to send to cancel a negation *)
val cancel_message : 'a result

(** The message send by the GC when a negation is collected
    (and will never be reachable again). *)
val finalize_message : 'a result

(** Cancel a session: traverse all sub-expressions to cancel negations. *)
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

(** Basic creation of a session.
    [greetings] and [cogreetings] are respectively the first session sent and
    received.
    The whole purpose of connect is to convert high-level sessions to plain
    values back and forth, by allocating and managing concrete addresses. *)
val connect :
  ?greetings:t -> ?cogreetings:(t -> unit) ->
  ?stderr:(_ error -> unit) -> output -> output * status

val close : output -> unit

val pending_sessions : status -> int
val is_closed : status -> bool
