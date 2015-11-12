open Ui_print

type t

val make : cursor -> string -> (t -> title:cursor -> body:cursor -> unit) -> unit

val modal : t -> string -> (t -> title:cursor -> body:cursor -> unit) -> unit
