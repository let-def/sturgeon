(* {{{ COPYING *(

   Emacs_sexp by Frédéric Bour <frederic.bour(_)lakaban.net>

   To the extent possible under law, the person who associated CC0 with
   Emacs_sexp has waived all copyright and related or neighboring rights
   to Emacs_sexp.

   You should have received a copy of the CC0 legalcode along with this
   work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

)* }}} *)

(** {1 Parametric S-exp} *)

type 'a sexp =
    C of 'a sexp * 'a sexp  (** cons cell   *)
  | S of string       (** 'sym        *)
  | T of string       (** "text"      *)
  | P of 'a sexp         (** #(property) *)
  | I of int          (** 1           *)
  | F of float        (** 1.0         *)
  | M of 'a           (** user-defined construction, outside of s-exp language *)

(** Recursively transform a sexp.
    [map] function is applied on each atom and at the root of each list *)
val transform_list : inj:('a -> 'b sexp) -> map:('b sexp -> 'b sexp) -> 'a sexp -> 'b sexp

(** Recursively transform a sexp.
    [map] function is applied on each atom and each cons-cell *)
val transform_cons : inj:('a -> 'b sexp) -> map:('b sexp -> 'b sexp) -> 'a sexp -> 'b sexp

(** nil constant: S "nil" *)
val sym_nil : 'a sexp

(** t constant: S "t" *)
val sym_t : 'a sexp

(** Build a sexp list,
    []      -> nil
    x :: xs -> C (x, sexp_of_list xs)
*)
val sexp_of_list : ?tail:'a sexp -> 'a sexp list -> 'a sexp

(** Does an element belong to a sexp list? *)
val sexp_mem : 'a sexp -> 'a sexp -> bool

(** {1 Monomorphic Emacs S-exp format} *)

type void

val void: void -> 'a

type basic = void sexp
(** {1 Low-level IO} *)

(** Serialize an s-exp by repetively calling a string printing function. *)
val tell_sexp : (string -> unit) -> basic -> unit

(** Read an basic by repetively calling a character reading function.

    The character reading function can return '\000' to signal EOF.

    Returns the basic and, if any, the last character read but not part of the
    sexp, or '\000'.

    If the basic is not well-formed, a Failure is raised.  You can catch it and
    add relevant location information.
    The error is always due to the last call to the reading function, which
    should be enough to locate the erroneous input, except for unterminated
    string.
*)
val read_sexp : (unit -> char) -> basic * char

(** {1 Higher-level IO} *)

val to_buf : basic -> Buffer.t -> unit

val to_string : basic -> string

val of_string : string -> basic

(** Read from a file descriptor.

    [on_read] is called before a potentially blocking read is done, so that you
    can act before blocking (select, notify scheduler ...).

    Partial application (stopping before the last [()]) allows to read a stream
    of sexp.
*)
val of_file_descr :
  on_read:(Unix.file_descr -> unit) -> Unix.file_descr -> unit -> basic option

(** Read from a channel.

    Partial application (stopping before the last [()]) allows to read a stream
    of sexp.
*)
val of_channel : in_channel -> unit -> basic option
