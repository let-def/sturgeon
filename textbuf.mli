open Sexp
open Inuit

type 'flags text = private {
  offset  : int;
  old_len : int;
  new_len : int;
  text    : string;
  flags   : 'flags list;
} constraint 'flags = [> flags]

val text_length : ?flags:[> flags] list -> string -> int
val text : ?flags:'flags list -> int -> int -> string -> 'flags text

class type ['flags] t =
  object
    method connect: 'flags t -> unit
    method change: 'flags text -> unit
    method click: int -> unit
    constraint 'flags = [> flags]
  end

type simple = flags t

val change : 'flags #t -> 'flags text -> unit
val click : 'flags #t -> int -> unit
val connect : a:'flags #t -> b:'flags #t -> unit

val null : [> flags] t

val with_cursor : unit -> 'flags cursor * 'flags t
