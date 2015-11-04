open Emacs_hyperprint

type t

val make : Emacs_hyperprint.cursor -> t
val add : ?children:(t -> unit) -> ?action:(cursor -> unit) -> t -> cursor
val clear : t -> unit
