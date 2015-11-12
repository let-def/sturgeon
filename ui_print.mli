open Sexp

type cursor
type action = cursor -> unit

val text : cursor -> ?properties:basic -> string -> unit
val clear : cursor -> unit
val sub : ?action:action option -> cursor -> cursor
val is_closed : cursor -> bool
val closed : cursor

val link : cursor -> ?properties:basic -> string -> action -> unit
val printf : cursor -> ?properties:basic -> ('a, unit, string, unit) format4 -> 'a

val open_buffer : Session.endpoint -> string -> cursor

val accept : Session.t -> cursor * (string -> unit)
