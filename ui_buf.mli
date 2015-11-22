open Sexp

val string_length : ?raw:bool -> string -> int

type buffer

val null : buffer

type text = {
  position  : int;
  old_len   : int;
  new_len   : int;
  text      : string;
  text_raw  : bool;
  clickable : bool;
}

val change : buffer -> ?raw:bool -> ?clickable:bool -> int -> int -> string -> unit

val click : buffer -> int -> unit

module Class : sig
  type 'a t = {
    connect: 'a -> buffer -> unit;
    connected: 'a -> unit;
    change: 'a -> text -> unit;
    click: 'a -> int -> unit;
  }
  val make : 'a t -> 'a -> buffer
end

val session : unit -> Session.t * buffer
