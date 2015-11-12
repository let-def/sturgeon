val command :
  ?greetings:Session.t ->
  ?cogreetings:(Session.t -> unit) ->
  unit -> 'a

val text_command :
  (args:Session.t ->
   set_title:(string -> unit) -> Ui_print.cursor -> unit) -> 'a
