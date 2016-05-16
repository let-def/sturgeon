val command :
  ?greetings:Session.t ->
  ?cogreetings:(Session.t -> unit) ->
  unit -> 'a

val text_command : (args:Session.t -> Stui.buffer_shell -> unit) -> 'a

type server

val server:
  client:(unit -> Session.t option * (Session.t -> unit) option) ->
  string -> server

val text_server:
  string ->
  (args:Session.t -> Stui.buffer_shell -> unit) ->
  server

val accept :
  server -> unit

val main_loop :
  server -> unit

val stop_server :
  server -> unit
