open Sexp

val string_length : ?raw:bool -> string -> int

type t

val create : unit -> Session.t * t

type editor
val cancel : editor -> unit

val edit : t ->
  on_change:(start:int -> old_len:int ->
             text_raw:bool -> text_len:int -> text:string ->
             clickable:bool -> unit) ->
  on_click:(int -> unit) ->
  editor

val change : editor ->
  start:int -> old_len:int -> text_raw:bool -> text:string -> clickable:bool -> unit

val click : editor -> int -> unit
