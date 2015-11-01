type sexp = Emacs_sexp.sexp
type cursor

val text : ?properties:sexp -> cursor -> string -> unit
val clear : cursor -> unit
val sub : ?action:(fun cursor -> unit) -> cursor -> cursor
