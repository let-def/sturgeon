(** {1 Emacs S-exp format} *)

type t =
    C of t * t  (** cons cell   *)
  | S of string (** 'sym        *)
  | T of string (** "text"      *)
  | P of t      (** #(property) *)
  | I of int    (** 1           *)
  | F of float  (** 1.0         *)

(** {2 Basic values} *)

(** nil constant: S "nil" *)
val nil : t

(** t constant: S "t" *)
val t : t

(** Build  a list in sexp format,
    []      -> nil
    x :: xs -> C (x, sexp_of_list xs)
*)
val sexp_of_list : t list -> t

(** {2 Low-level IO} *)
val tell_sexp : (string -> unit) -> t -> 'a
val read_sexp : (unit -> char) -> t * char option

(** {2 Higher-level IO} *)

val to_buf : t -> Buffer.t -> unit

val to_string : t -> string

val of_string : string -> t

(** Read from a file descriptor.

    [on_read] is called before a potentially blocking read is done, so that you
    can act before blocking (select, notify scheduler ...).

    Partial application (stopping before the last [()]) allows to read a stream
    of sexp.
*)
val of_file_descr :
  on_read:(Unix.file_descr -> unit) -> Unix.file_descr -> unit -> t option

(** Read from a channel.

    Partial application (stopping before the last [()]) allows to read a stream
    of sexp.
*)
val of_channel : in_channel -> unit -> t option
