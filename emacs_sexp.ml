(* {{{ COPYING *(

   Emacs_sexp by Frédéric Bour <frederic.bour(_)lakaban.net>

   To the extent possible under law, the person who associated CC0 with
   Emacs_sexp has waived all copyright and related or neighboring rights
   to Emacs_sexp.

   You should have received a copy of the CC0 legalcode along with this
   work.  If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.

)* }}} *)

type 'a sexp =
    C of 'a sexp * 'a sexp
  | S of string
  | T of string
  | P of 'a sexp
  | I of int
  | F of float
  | M of 'a

type void
let void (_ : void) = assert false

let transform_list ~inj ~map t =
  let rec aux = function
    | S _ | T _ | I _ | F _ as t' -> map t'
    | C (a, b) -> map (C (aux a, aux_cons b))
    | P a -> map (P (aux a))
    | M x -> inj x
  and aux_cons = function
    | C (a, b) -> C (aux a, aux_cons b)
    | S "nil" as t -> t
    | t -> aux t
  in
  aux t

let transform_cons ~inj ~map t =
  let rec aux = function
    | S _ | T _ | I _ | F _ as t' -> map t'
    | C (a, b) -> map (C (aux a, aux b))
    | P a -> map (P (aux a))
    | M x -> inj x
  in
  aux t


type basic = void sexp

let sym_t = S "t"
let sym_nil = S "nil"

let sexp_of_list ?(tail=sym_nil) l =
  let rec aux =  function
    | [] -> tail
    | a :: tl -> C (a, aux tl)
  in
  aux l

let rec tell_sexp (tell : _ -> unit) sexp =
  match sexp with
  | C (a,b) ->
    tell "(";
    tell_sexp tell a;
    tell_cons tell b
  | T s -> tell ("\"" ^ String.escaped s ^ "\"")
  | S s -> tell (String.escaped s)
  | I i -> tell (string_of_int i)
  | F f -> tell (string_of_float f)
  | P s -> tell "#"; tell_sexp tell s
  | M v -> void v


and tell_cons tell = function
  | S "nil" -> tell ")"
  | C (a,b) ->
    tell " ";
    tell_sexp tell a;
    tell_cons tell b
  | sexp ->
    tell " . ";
    tell_sexp tell sexp;
    tell ")"

let is_alpha c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')

let is_num c =
  (c >= '0' && c <= '9')

let is_alphanum c = is_alpha c || is_num c

let read_sexp getch =
  let buf = Buffer.create 10 in
  let rec read_sexp = function
    | ' ' | '\t' | '\n' ->
      read_sexp (getch ())

    | c when is_num c ->
      read_num c

    | '\'' | ':' | '_' | '\\' as c -> read_sym c
    | c when is_alpha c -> read_sym c

    | '"' -> read_string ()

    | '\000' -> raise End_of_file
    | '(' ->
      let lhs, next = read_sexp (getch ()) in
      read_cons [lhs] next

    | '#' ->
      let t, c = read_sexp (getch ()) in
      P t, c

    | _ -> failwith "Invalid parse"

  and read_cons cells = function
    | ' ' | '\t' | '\n' -> read_cons cells (getch ())
    | ')' -> sexp_of_list (List.rev cells), '\000'
    | '.' ->
      let rhs, c = read_sexp (getch ()) in
      let rec aux = function
        | ')' -> sexp_of_list ~tail:rhs (List.rev cells)
        | ' ' | '\t' | '\n' -> aux (getch ())
        | _ -> failwith "Invalid parse"
      in
      aux (if c = '\000' then getch() else c), '\000'
    | c ->
      let cell, c = read_sexp c in
      read_cons
        (cell :: cells)
        (if c = '\000' then getch() else c)

  and read_num c =
    Buffer.clear buf;
    Buffer.add_char buf c;
    let rec aux is_float =
      match getch () with
      | c when c >= '0' && c <= '9' ->
        Buffer.add_char buf c; aux is_float
      | '.' | 'e' | 'E' as c ->
        Buffer.add_char buf c; aux true
      | c ->
        let s = Buffer.contents buf in
        (if is_float
         then F (float_of_string s)
         else I (int_of_string s)),
        c
    in
    aux false

  and read_string () =
    Buffer.clear buf;
    let rec aux = function
      | '\000' -> failwith "Unterminated string"
      | '\\' ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf (getch ());
        aux (getch ())
      | '"' ->
        T (Scanf.unescaped (Buffer.contents buf)), '\000'
      | c ->
        Buffer.add_char buf c;
        aux (getch ())
    in
    aux (getch ())

  and read_sym c =
    Buffer.clear buf;
    let rec aux = function
      | ('\'' | '-' | ':' | '_') as c ->
        Buffer.add_char buf c;
        aux (getch ())
      | c when is_alphanum c ->
        Buffer.add_char buf c;
        aux (getch ())
      | '\\' ->
        Buffer.add_char buf (getch ());
        aux (getch ())
      | c -> S (Buffer.contents buf), c
    in
    aux (if c = '\000' then getch() else c)
  in
  read_sexp (getch ())

let to_buf sexp buf =
  tell_sexp (Buffer.add_string buf) sexp

let to_string sexp =
  let buf = Buffer.create 100 in
  to_buf sexp buf;
  Buffer.contents buf

let getch_of_substring str pos len =
  let len = pos + len in
  if pos < 0 || len > String.length str then
    invalid_arg "Sexp.getch_of_substring";
  let pos = ref pos in
  let getch () =
    if !pos < len then
      let r = str.[!pos] in
      incr pos;
      r
    else '\000'
  in
  getch

let getch_of_string str =
  getch_of_substring str 0 (String.length str)

let of_string str =
  fst (read_sexp (getch_of_string str))

let of_file_descr ~on_read fd =
  let getch = ref (fun () -> '\000') in
  let rest = ref '\000' in
  let buffer = Bytes.create 1024 in
  let getch () =
    match !rest with
    | '\000' ->
      begin match !getch () with
        | '\000' ->
          (on_read fd : unit);
          let read = Unix.read fd buffer 0 1024 in
          if read = 0 then '\000'
          else
            begin
              getch := getch_of_substring buffer 0 read;
              !getch ()
            end
        | c -> c
      end
    | c -> rest := '\000'; c
  in
  fun () ->
    try
      let sexp, rest' = read_sexp getch in
      rest := rest';
      Some sexp
    with End_of_file -> None

let of_channel ic =
  let getch = ref (fun () -> '\000') in
  let rest = ref '\000' in
  let buffer = Bytes.create 1024 in
  let getch () =
    match !rest with
    | '\000' ->
      begin match !getch () with
        | '\000' ->
          let read = input ic buffer 0 1024 in
          if read = 0 then '\000'
          else
            begin
              getch := getch_of_substring buffer 0 read;
              !getch ()
            end
        | c -> c
      end
    | c -> rest := '\000'; c
  in
  fun () ->
    try
      let sexp, rest' = read_sexp getch in
      rest := rest';
      Some sexp
    with End_of_file -> None
