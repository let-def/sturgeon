type 'a t and 'a cursor

val create : ?on_invalidate:('a cursor -> unit) -> unit -> 'a t

val clear : 'a t -> unit
val is_empty : 'a t -> bool

val remove : 'a t -> at:int -> len:int -> unit
val insert : 'a t -> at:int -> len:int -> unit

val remove_between : 'a cursor -> 'a cursor -> unit
val remove_after : 'a cursor -> int -> unit
val remove_before : 'a cursor -> int -> unit

val insert_before : 'a cursor -> int -> unit
val insert_after : 'a cursor -> int -> unit

val put_cursor : 'a t -> at:int -> 'a -> 'a cursor
val rem_cursor : 'a cursor -> unit

val before     : 'a cursor -> 'a -> 'a cursor
val after      : 'a cursor -> 'a -> 'a cursor

val buffer     : 'a cursor -> 'a t
val content    : 'a cursor -> 'a
val position   : 'a cursor -> int
val valid      : 'a cursor -> bool

val find_before : 'a t -> int -> 'a cursor option
val find_after : 'a t -> int -> 'a cursor option
val cursor_before : 'a cursor -> 'a cursor option
val cursor_after : 'a cursor -> 'a cursor option
