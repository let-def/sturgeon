open Sexp

type cursor
type action = cursor -> unit

val text : cursor -> ?raw:bool -> ?properties:basic -> string -> unit
val clear : cursor -> unit
val sub : ?action:action option -> cursor -> cursor
val is_closed : cursor -> bool
val closed : cursor

val link : cursor -> ?raw:bool -> ?properties:basic -> string -> action -> unit
val printf : cursor -> ?raw:bool -> ?properties:basic -> ('a, unit, string, unit) format4 -> 'a

val buffer_greetings : string -> Session.t * cursor

val accept : Session.t -> cursor * (string -> unit)
