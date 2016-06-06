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

(** An API to interconnect Sturgeon and Inuit. *)

open Inuit

(** Set of flags recognized by Sturgeon interface. *)
type flag = [`Clickable | `Clicked | `Editable | `Invisible]

(** A buffer shell is the abstract object representing the connection
    to the user-interface display.
    Through a shell, you can create one or more buffers.

    TODO:
    - bind [message] function from emacs for reporting information *)
type buffer_shell

(** [buffer_greetings] provide you with a session and a shell.
    Send the session as a greetings to emacs instance and the shell will
    open and display buffers in this instance. *)
val buffer_greetings :
  unit -> Session.t * buffer_shell

(** Create patch socket (low-level) from a shell. *)
val create_buffer : buffer_shell -> name:string -> flag patch socket -> unit

(** Create cursor ready to output in a buffer. *)
val create_cursor : buffer_shell -> name:string -> flag cursor

(** FIXME: Document other protocols *)
val accept_buffer : Session.t -> flag patch socket -> unit
val accept_cursor : Session.t -> flag cursor
