open Sexp

val string_length : ?raw:bool -> string -> int

type t

val create : unit -> Session.t * t

type 'a change =
  start:int -> old_len:int ->
  text_raw:bool -> text_len:int -> text:string -> clickable:bool -> 'a

val remote_changes :
  t ->
  (start:int -> old_len:int ->
   text_raw:bool -> text_len:int -> text:string -> clickable:bool -> [`Keep | `Drop]) -> unit

val remote_clicks :
  t -> (int -> [`Keep | `Drop]) -> unit

val local_change :
  t -> start:int -> old_len:int -> text_raw:bool -> text:string -> clickable:bool -> unit

val local_click : t -> int -> unit
