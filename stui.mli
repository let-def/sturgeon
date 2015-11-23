open Tui

val textbuf_session : unit -> Session.t * Textbuf.t

val cursor_greetings : name:string -> Session.t * cursor

val accept_textbuf : Session.t -> textbuf * (string -> unit)
val accept_cursor : Session.t -> cursor * (string -> unit)
