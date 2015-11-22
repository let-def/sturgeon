open Sexp

type cursor
type action = cursor -> unit

val text : cursor -> ?raw:bool -> ?properties:basic -> string -> unit
val clear : cursor -> unit
val sub : ?action:action option -> cursor -> cursor
val is_closed : cursor -> bool

val link : cursor -> ?raw:bool -> ?properties:basic -> string -> action -> unit
val printf : cursor -> ?raw:bool -> ?properties:basic -> ('a, unit, string, unit) format4 -> 'a

(* Creating cursors *)

val buffer_greetings : name:string -> Session.t * cursor
val accept : Session.t -> cursor * (string -> unit)

val closed : cursor

module Class : sig
  type 'a t = {
    text      : 'a -> ?raw:bool -> ?properties:basic -> string -> unit;
    clear     : 'a -> unit;
    sub       : ?action:action option -> 'a -> cursor;
    is_closed : 'a -> bool;
  }
  val make : 'a t -> 'a -> cursor
end
