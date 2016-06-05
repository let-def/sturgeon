open Inuit

type flag = [`Clickable | `Clicked | `Editable | `Invisible]
type buffer_shell

val buffer_greetings :
  unit -> Session.t * buffer_shell

val create_buffer : buffer_shell -> name:string -> flag patch pipe -> unit
val create_cursor : buffer_shell -> name:string -> flag cursor

val accept_buffer : Session.t -> flag patch pipe -> unit
val accept_cursor : Session.t -> flag cursor
