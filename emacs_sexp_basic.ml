(* {{{ COPYING *(

   Emacs_sexp by Frédéric Bour <frederic.bour(_)lakaban.net>

   To the extent possible under law, the person who associated CC0 with
   Emacs_sexp has waived all copyright and related or neighboring rights
   to Emacs_sexp.

   You should have received a copy of the CC0 legalcode along with this
   work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

)* }}} *)

type void
let void _ = assert false

type t = void Emacs_sexp.t

let nil = Emacs_sexp.nil

let t = Emacs_sexp.t

let sexp_of_list = Emacs_sexp.sexp_of_list

let meta x = x

let tell_sexp tell t = Emacs_sexp.tell_sexp tell ~meta t
let to_buf t buf = Emacs_sexp.to_buf ~meta t buf
let to_string t = Emacs_sexp.to_string ~meta t

let read_sexp getch = Emacs_sexp.read_sexp ~meta getch
let of_string s = Emacs_sexp.of_string ~meta s
let of_file_descr ~on_read fd =
  Emacs_sexp.of_file_descr ~on_read ~meta fd
let of_channel ic = Emacs_sexp.of_channel ~meta ic
