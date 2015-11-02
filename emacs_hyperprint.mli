open Emacs_sexp

type cursor

val text : cursor -> ?properties:basic -> string -> unit
val clear : cursor -> unit
val sub : ?action:(cursor -> unit) -> cursor -> cursor

val link : cursor -> ?properties:basic -> string -> (cursor -> unit) -> unit
val printf : cursor -> ?properties:basic -> ('a, unit, string, unit) format4 -> 'a

val open_buffer : Emacs_serge.endpoint -> string -> cursor
