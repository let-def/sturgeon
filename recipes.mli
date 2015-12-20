val command :
  ?greetings:Session.t ->
  ?cogreetings:(Session.t -> unit) ->
  unit -> 'a

val text_command :
  (args:Session.t ->
   set_title:(string -> unit) -> Tui.cursor -> unit) -> 'a

type server

val server:
  ?greetings:(unit -> Session.t) ->
  ?cogreetings:(Session.t -> unit) ->
  string -> server

val text_server:
  string ->
  (args:Session.t ->
   set_title:(string -> unit) -> Tui.cursor -> unit) ->
  server

val accept:
  server -> unit Lwt.t
