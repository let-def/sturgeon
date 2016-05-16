type flag = [`Clickable | `Editable | `Clicked]
type buffer_shell = name:string -> flag Inuit.pipe -> unit

val buffer_greetings :
  unit -> Session.t * buffer_shell

val create_buffer : buffer_shell -> name:string -> flag Inuit.pipe -> unit
val create_cursor : buffer_shell -> name:string -> flag Inuit.cursor

val accept_buffer : Session.t -> flag Inuit.pipe -> unit
val accept_cursor : Session.t -> flag Inuit.cursor
