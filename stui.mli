val textbuf_session : unit -> Session.t * Textbuf.simple

val cursor_greetings : name:string -> Session.t * Inuit.flags Inuit.cursor

val accept_textbuf : Session.t -> Textbuf.simple * (string -> unit)
val accept_cursor : Session.t -> Inuit.flags Inuit.cursor * (string -> unit)
