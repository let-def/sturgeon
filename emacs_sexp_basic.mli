(* {{{ COPYING *(

   Emacs_sexp by Frédéric Bour <frederic.bour(_)lakaban.net>

   To the extent possible under law, the person who associated CC0 with
   Emacs_sexp has waived all copyright and related or neighboring rights
   to Emacs_sexp.

   You should have received a copy of the CC0 legalcode along with this
   work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

)* }}} *)

(** {1 Emacs S-exp format} *)

type void
val void: void -> 'a
 
type t = void Emacs_sexp.t

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

(** Serialize an s-exp by repetively calling a string printing function. *)
val tell_sexp : (string -> unit) -> t -> unit

(** Read an sexp by repetively calling a character reading function.

    The character reading function can return '\000' to signal EOF.

    Returns the sexp and, if any, the last character read but not part of the
    sexp, or '\000'.

    If the sexp is not well-formed, a Failure is raised.  You can catch it and
    add relevant location information.
    The error is always due to the last call to the reading function, which
    should be enough to locate the erroneous input, except for unterminated
    string.
*)
val read_sexp : (unit -> char) -> t * char

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
