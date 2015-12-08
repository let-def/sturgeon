open Sexp

type cursor
type textbuf

(* Minimal text API *)

type action = cursor -> unit

val text   : cursor -> ?raw:bool -> ?properties:basic -> string -> unit
val clear  : cursor -> unit
val sub    : ?action:action option -> cursor -> cursor
val link   : cursor -> ?raw:bool -> ?properties:basic ->
             string -> action -> unit
val printf : cursor -> ?raw:bool -> ?properties:basic ->
             ('a, unit, string, unit) format4 -> 'a

val null_cursor : cursor
val is_closed   : cursor -> bool

(* Basic widgets *)

module Nav : sig
  type t
  val make  : cursor -> string -> (t -> unit) -> unit
  val modal : t -> string -> (t -> unit) -> unit

  val title : t -> cursor
  val body  : t -> cursor

  val null : t
end

module Tree : sig
  type t
  val make : cursor -> t
  val add : ?children:(t -> unit) -> ?action:action option -> ?opened:bool ref -> t -> cursor
  val clear : t -> unit

  val null : t
end

(* Low-level extension *)

module Textbuf : sig
  type t = textbuf

  val string_length : ?raw:bool -> string -> int

  type text = private {
    position  : int;
    old_len   : int;
    new_len   : int;
    text      : string;
    text_raw  : bool;
    clickable : bool;
    editable  : bool;
  }

  val text :
    ?raw:bool -> ?clickable:bool -> ?editable:bool ->
    int -> int -> string -> text

  val null : t
  val change : t -> text -> unit
  val click : t -> int -> unit
  val connect : a:t -> b:t -> unit

  val with_cursor : unit -> cursor * textbuf
end

module Class : sig
  type 'a textbuf = {
    connect: 'a -> Textbuf.t -> unit;
    connected: 'a -> unit;
    change: 'a -> Textbuf.text -> unit;
    click: 'a -> int -> unit;
  }

  val make_textbuf : 'a textbuf -> 'a -> Textbuf.t

  type cursor' = cursor

  type 'a cursor = {
    text      : 'a -> ?raw:bool -> ?properties:basic -> string -> unit;
    clear     : 'a -> unit;
    sub       : ?action:action option -> 'a -> cursor';
    is_closed : 'a -> bool;
  }

  val make_cursor : 'a cursor -> 'a -> cursor'
end

