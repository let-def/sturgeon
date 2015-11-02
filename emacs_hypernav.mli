open Emacs_hyperprint

type t

val navigator : cursor -> string -> (t -> title:cursor -> body:cursor -> unit) -> unit

val goto : t -> string -> (t -> title:cursor -> body:cursor -> unit) -> unit
