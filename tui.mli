open Sexp

type cursor
type textbuf

(* Minimal text API *)

module Cursor : sig
  type t = cursor
  type action = t -> unit

  val text  : t -> ?raw:bool -> ?properties:basic -> string -> unit
  val clear : t -> unit
  val sub   : ?action:action option -> t -> t
  val is_closed : t -> bool

  val link   : t -> ?raw:bool -> ?properties:basic -> string -> action -> unit
  val printf : t -> ?raw:bool -> ?properties:basic ->
               ('a, unit, string, unit) format4 -> 'a

  val closed : t

  val in_textbuf : unit -> cursor * textbuf
end

(* Basic widgets *)

module Nav : sig
  type t
  val make  : cursor -> string -> (t -> cursor -> unit) -> unit
  val title : t -> cursor
  val modal : t -> string -> (t -> cursor -> unit) -> unit
end

module Tree : sig
  type t
  val make : cursor -> t
  val add : ?children:(t -> unit) -> ?action:Cursor.action option -> t -> cursor
  val clear : t -> unit
end

(* Low-level extension *)

module Textbuf : sig
  type t = textbuf

  val string_length : ?raw:bool -> string -> int

  type text = {
    position  : int;
    old_len   : int;
    new_len   : int;
    text      : string;
    text_raw  : bool;
    clickable : bool;
  }

  val null : t
  val change : t -> ?raw:bool -> ?clickable:bool -> int -> int -> string -> unit
  val click : t -> int -> unit
  val connect : a:t -> b:t -> unit
end

module Class : sig
  type 'a textbuf = {
    connect: 'a -> Textbuf.t -> unit;
    connected: 'a -> unit;
    change: 'a -> Textbuf.text -> unit;
    click: 'a -> int -> unit;
  }

  val make_textbuf : 'a textbuf -> 'a -> Textbuf.t

  type 'a cursor = {
    text      : 'a -> ?raw:bool -> ?properties:basic -> string -> unit;
    clear     : 'a -> unit;
    sub       : ?action:Cursor.action option -> 'a -> Cursor.t;
    is_closed : 'a -> bool;
  }

  val make_cursor : 'a cursor -> 'a -> Cursor.t
end

