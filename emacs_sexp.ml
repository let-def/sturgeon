(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

type t =
  | C of t * t
  | S of string
  | T of string
  | P of t
  | I of int
  | F of float

let t = S "t"
let nil = S "nil"

let rec sexp_of_list = function
  | [] -> nil
  | a :: tl -> C (a, sexp_of_list tl)

let rec tell_sexp (tell : _ -> unit) = function
  | C (a,b) ->
    tell "(";
    tell_sexp tell a;
    tell_cons tell b
  | T s -> tell ("\"" ^ String.escaped s ^ "\"")
  | S s -> tell (String.escaped s)
  | I i -> tell (string_of_int i)
  | F f -> tell (string_of_float f)
  | P s -> tell "#"; tell_sexp tell s


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
  let rec read_sexp getch = function
    | ' ' | '\t' | '\n' ->
      read_sexp getch (getch ())

    | c when is_num c ->
      read_num getch c

    | '\'' | ':' | '_' | '\\' as c -> read_sym getch (Some c)
    | c when is_alpha c -> read_sym getch (Some c)

    | '"' ->
      read_string getch
    | '\000' -> raise End_of_file
    | '(' ->
      let lhs, next = read_sexp getch (getch ()) in
      read_cons getch (fun rhs -> C (lhs, rhs)) next

    | '#' ->
      let t, c = read_sexp getch (getch ()) in
      P t, c

    | _ -> failwith "Invalid parse"

  and read_cons getch k next =
    match (match next with Some c -> c | None -> getch ()) with
    | ' ' | '\t' | '\n' -> read_cons getch k None
    | ')' -> k nil, None
    | '.' ->
      let rhs, next = read_sexp getch (getch ()) in
      let rec aux = function
        | ')' -> k rhs
        | ' ' | '\t' | '\n' -> aux (getch ())
        | _ -> failwith "Invalid parse"
      in
      begin match next with
        | Some c -> aux c
        | None -> aux (getch ())
      end, None
    | c ->
      let cell, next = read_sexp getch c in
      read_cons getch (fun rhs -> k (C (cell, rhs))) next

  and read_num getch c =
    Buffer.clear buf;
    Buffer.add_char buf c;
    let is_float = ref false in
    let rec aux () =
      match getch () with
      | c when c >= '0' && c <= '9' ->
        Buffer.add_char buf c; aux ()
      | '.' | 'e' | 'E' as c ->
        is_float := true;
        Buffer.add_char buf c; aux ()
      | c ->
        let s = Buffer.contents buf in
        (if !is_float
         then F (float_of_string s)
         else I (int_of_string s)),
        Some c
    in
    aux ()

  and read_string getch =
    Buffer.clear buf;
    let rec aux () =
      match getch () with
      | '\000' -> failwith "Unterminated string"
      | '\\' ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf (getch ());
        aux ()
      | '"' ->
        T (Scanf.unescaped (Buffer.contents buf)), None
      | c ->
        Buffer.add_char buf c;
        aux ()
    in
    aux ()

  and read_sym getch next =
    Buffer.clear buf;
    let rec aux next =
      match (match next with Some c -> c | None -> getch ()) with
      | ('\'' | '-' | ':' | '_') as c ->
        Buffer.add_char buf c;
        aux None
      | c when is_alphanum c ->
        Buffer.add_char buf c;
        aux None
      | '\\' ->
        Buffer.add_char buf (getch ());
        aux None
      | c -> S (Buffer.contents buf), Some c
    in
    aux next
  in
  read_sexp getch (getch ())

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
  let rest = ref None in
  let buffer = Bytes.create 1024 in
  let getch () =
    match !rest with
    | Some r ->
      rest := None;
      r
    | None ->
      match !getch () with
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
  in
  fun () ->
    try
      let sexp, rest' = read_sexp getch in
      rest := rest';
      Some sexp
    with End_of_file -> None

let of_channel ic =
  let getch = ref (fun () -> '\000') in
  let rest = ref None in
  let buffer = Bytes.create 1024 in
  let getch () =
    match !rest with
    | Some r ->
      rest := None;
      r
    | None ->
      match !getch () with
      | '\000' ->
        let read = input ic buffer 0 1024 in
        if read = 0 then '\000'
        else
          begin
            getch := getch_of_substring buffer 0 read;
            !getch ()
          end
      | c -> c
  in
  fun () ->
    try
      let sexp, rest' = read_sexp getch in
      rest := rest';
      Some sexp
    with End_of_file -> None
