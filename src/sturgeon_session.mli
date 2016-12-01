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

open Result
open Sturgeon_sexp

(** Session setup a connection between two parties and extends Sexp with
    continuations from the remote party.
    One can put function-like values inside these Sexp, which, when applied,
    invoke some remote code.

    The library take care of allocating simple handles to reference
    continuation from remote party.
*)

(** The reason for a continuation to not be invoked anymore.
    `Cancel: the continuation was not expected, consumer does not know how to
      deal with it.
    `Finalize: the GC determined that the continuation cannot be reached
      anymore.
    `Other: some error defined by implementor of remote code. This is analogous
      to exceptions.
      The error message is a plain Sexp: it is not possible to "catch" the
      error and resume control flow.
*)
type reason = [ `Cancel | `Finalize | `Other of basic ]

(** A continuation either takes a result value or a reason for terminating. *)
type 'a cont = ('a, reason) result -> unit

(** Remote values.

    Basic Sexp contain plain values that you can inspect.
    Sessions also contain remote values. Those are opaque but can consume
    values that you produce and send to the remote side.

    Positive and negative values are the building blocks of more complex
    control flows.
    For instance, a function from ['a -> 'b] can be encoded as
      [('a * 'b remote) remote]:
    if you give an ['a], you will be given a ['b].
*)
type remote =
  | Once of t cont (** [Once] is the constructor for remote linear
                       continuations: they can consume only one value. *)
  | Many of t cont (** [Many] is the constructor for remote multi-shot
                       continuations: they can consume arbitrarily many values.
                       They can be used to build streams. *)

(** Finally the type of sessions: it is the S-exp extended with remote values. *)
and t = remote sexp

type 'a error =
  [ `Already_closed  of (t, reason) result
  | `Query_after_eof of t
  | `Invalid_command of basic
  | `Feed_unknown    of basic
  | `Quit_unknown    of basic
  | `Exceptions_during_cancellation of t * exn list
  | `Exceptions_during_shutdown of exn list
  ]

(** Cancel a session: traverse all sub-expressions to cancel continuations. *)
val cancel :
  ?stderr:([> `Exceptions_during_cancellation of t * exn list] -> unit) ->
  t -> unit

type output = basic -> unit
type status

(** Basic creation of a session.
    [greetings] and [cogreetings] are respectively the first session sent and
    received.
    The purpose of connect is to convert high-level sessions back and forth
    plain values, by allocating and managing concrete addresses. *)
val connect :
  ?greetings:t -> ?cogreetings:(t -> unit) ->
  ?stderr:(_ error -> unit) -> output -> output * status

val close : output -> unit

val pending_continuations : status -> int

val is_closed : status -> bool
