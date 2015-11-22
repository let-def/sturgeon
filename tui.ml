open Sexp
open Session

type text' = {
  position  : int;
  old_len   : int;
  new_len   : int;
  text      : string;
  text_raw  : bool;
  clickable : bool;
}

module Class = struct
  type 'a textbuf = {
    connect: 'a -> textbuf' -> unit;
    connected: 'a -> unit;
    change: 'a -> text' -> unit;
    click: 'a -> int -> unit;
  }
  and textbuf' = Textbuf : 'a textbuf * 'a -> textbuf'
  let make_textbuf c i = Textbuf (c, i)

  type 'a cursor = {
    text      : 'a -> ?raw:bool -> ?properties:basic -> string -> unit;
    clear     : 'a -> unit;
    sub       : ?action:action option -> 'a -> cursor';
    is_closed : 'a -> bool;
  }
  and cursor' = Cursor : 'a cursor * 'a -> cursor'
  and action = cursor' -> unit
  let make_cursor c i = Cursor (c, i)
end

type textbuf = Class.textbuf'
type cursor  = Class.cursor'

module Textbuf = struct
  type t = textbuf

  type text = text' = {
    position  : int;
    old_len   : int;
    new_len   : int;
    text      : string;
    text_raw  : bool;
    clickable : bool;
  }

  let string_length ?(raw=false) str =
    if raw then String.length str
    else
      let count = ref 0 in
      for i = 0 to String.length str - 1 do
        let c = Char.code str.[i] in
        if c land 0xC0 <> 0x80 then
          incr count
      done;
      !count

  let connect ~a ~b =
    let Class.Textbuf (ca, ia) = a in
    let Class.Textbuf (cb, ib) = b in
    ca.Class.connect ia b;
    cb.Class.connect ib a;
    ca.Class.connected ia;
    cb.Class.connected ib

  let click (Class.Textbuf (c, i)) offset =
    c.Class.click i offset

  let change (Class.Textbuf (c, i))
      ?(raw=false) ?(clickable=false) start len text =
    let text = {
      position  = start;
      old_len   = len;
      new_len   = string_length ~raw text;
      text      = text;
      text_raw  = raw;
      clickable = clickable;
    } in
    c.Class.change i text

  let null = Class.Textbuf ({
      Class.
      connect   = (fun () _ -> ());
      connected = (fun () -> ());
      change    = (fun () _text -> ());
      click     = (fun () _offset -> ());
    }, ())
end

module Cursor = struct
  type t = cursor
  type action = Class.action

  let is_closed (Class.Cursor (c, i)) =
    c.Class.is_closed i

  let text (Class.Cursor (c, i)) ?raw ?properties txt =
    c.Class.text i ?raw ?properties txt

  let clear (Class.Cursor (c, i)) =
    c.Class.clear i

  let sub ?action (Class.Cursor (c, i)) =
    c.Class.sub ?action i

  let link cursor ?raw ?properties msg action =
    let cursor = sub ~action:(Some action) cursor in
    text cursor ?raw ?properties msg

  let printf (cursor : cursor) ?raw ?properties fmt =
    Printf.ksprintf (text cursor ?raw ?properties) fmt

  let rec closed = Class.Cursor ({
      Class.
      text      = (fun () ?raw:_ ?properties:_ _ -> ());
      clear     = (fun () -> ());
      sub       = (fun ?action:_ () -> closed);
      is_closed = (fun () -> true);
    }, ())
end

module Nav = struct
  open Cursor

  type t = {
    mutable prev: page list;
    mutable page: page;
    mutable next: page list;

    title: cursor;
    body: cursor;
  }

  and page = string * (t -> cursor -> unit)

  let refresh t =
    clear t.title;
    text t.title (fst t.page);

    clear t.body;
    (snd t.page) t t.body

  let next t =
    match t.next with
    | [] -> ()
    | page :: pages ->
      t.prev <- t.page :: t.prev;
      t.page <- page;
      t.next <- pages;
      refresh t

  let prev t =
    match t.prev with
    | [] -> ()
    | page :: pages ->
      t.next <- t.page :: t.next;
      t.page <- page;
      t.prev <- pages;
      refresh t

  let render_header t cursor =
    link cursor "<<" (fun _ -> prev t);
    text cursor " ";
    link cursor "[?]" (fun _ -> refresh t);
    text cursor " ";
    link cursor ">>" (fun _ -> next t)

  let make cursor label content =
    let header = sub cursor in
    text cursor " ";
    let title = sub cursor in
    text cursor "\n\n";
    let body = sub cursor in
    let t = { prev = []; page = (label, content); next = []; title; body } in
    render_header t header;
    refresh t

  let title t =
    t.title

  let modal t label content =
    t.next <- [(label, content)];
    next t
end

module Tree = struct
  open Cursor

  type t = {
    indent: int;
    cursor: cursor;
  }

  let make cursor =
    { indent = 0; cursor = sub cursor }

  let indent t =
    if t.indent > 0 then
      text t.cursor (String.make t.indent ' ')

  let add_leaf ?action t =
    indent t;
    text t.cursor "[ ] ";
    let result = sub ?action t.cursor in
    text t.cursor "\n";
    result

  let add_node children ?action t =
    indent t;
    let body = ref None in
    let opened = ref false in
    link t.cursor "[+]" (fun c ->
        match !body with
        | None -> ()
        | Some t' when !opened ->
          opened := false;
          clear c; text c "[+]";
          clear t'.cursor
        | Some t' ->
          opened := true;
          clear c; text c "[-]";
          children t'
      );
    text t.cursor " ";
    let result = sub ?action t.cursor in
    text t.cursor "\n";
    body := Some { indent = t.indent + 1; cursor = sub t.cursor };
    result

  let add ?children ?action t =
    match children with
    | None -> add_leaf ?action t
    | Some children -> add_node children ?action t

  let clear t = clear t.cursor
end
