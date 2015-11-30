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

type cursor  = Class.cursor'
type textbuf = Class.textbuf'
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

let rec null_cursor = Class.Cursor ({
    Class.
    text      = (fun () ?raw:_ ?properties:_ _ -> ());
    clear     = (fun () -> ());
    sub       = (fun ?action:_ () -> null_cursor);
    is_closed = (fun () -> true);
  }, ())

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

  let direct_change (Class.Textbuf (c, i)) text =
    c.Class.change i text

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

  module Cursor = struct
    type t = {
      action : action option;
      trope  : t lazy_t Trope.t ref;
      mutable textbuf : textbuf;
      beginning : t lazy_t Trope.cursor;
      position  : t lazy_t Trope.cursor;
    }

    let get_action c = (Lazy.force (Trope.content c)).action

    let is_closed cursor = not (Trope.member !(cursor.trope) cursor.position)

    let sub' ?action current =
      if is_closed current then current
      else
        let action = match action with
          | None -> get_action current.beginning
          | Some action -> action
        in
        let rec cursor = lazy begin
          let {textbuf; trope; position} = current in
          let t, beginning = Trope.put_before !trope position  cursor in
          let t, position  = Trope.put_after  t beginning cursor in
          trope := t;
          {trope; textbuf; action; beginning; position}
        end in
        Lazy.force cursor

    let text {action; textbuf; trope; beginning; position} ?raw ?properties text =
      (*let cmd = match properties with
        | None -> [S "text"; T text]
        | Some props ->
          let props = transform_list ~inj:void ~map:(fun x -> x) props in
          [S "text"; T text; S ":properties"; props]
        in*)
      if Trope.member !trope position then begin
        let start = Trope.position !trope position in
        let length = string_length ?raw text in
        change textbuf start 0 ?raw ~clickable:(action <> None) text;
        trope := Trope.insert_before !trope position length
      end

    let clear {textbuf; trope; beginning; position} =
      if Trope.member !trope position then begin
        let start = Trope.position !trope beginning in
        let length = Trope.position !trope position - start in
        change textbuf start length "" ~raw:true ~clickable:false;
        trope := Trope.remove_between !trope beginning position
      end

    let rec cursor_class = {Class. text; sub; clear; is_closed}
    and sub ?action current =
      Class.Cursor (cursor_class, sub' ?action current)

    let textbuf_class = {
      Class.
      connect = (fun t buffer -> t.textbuf <- buffer);
      connected = ignore;
      change = (fun t text ->
          let trope = t.trope in
          if text.old_len <> 0 then
            trope := Trope.remove ~at:text.position ~len:text.old_len !trope;
          if text.new_len <> 0 then
            trope := Trope.insert ~at:text.position ~len:text.new_len !trope
        );
      click = (fun t offset ->
          match Trope.find_before !(t.trope) offset with
          | None -> exit 4
          | Some cursor ->
            match get_action cursor with
            | None -> exit 5
            | Some action ->
              action (Class.Cursor
                        (cursor_class, (Lazy.force (Trope.content cursor))))
        );
    }

    let create () =
      let trope = ref (Trope.create ()) in
      let rec cursor = lazy begin
        let t, beginning = Trope.put_cursor !trope ~at:0 cursor in
        let t, position  = Trope.put_after  t beginning cursor in
        trope := t;
        {textbuf = null; beginning; position; trope; action = None}
      end in
      let lazy cursor = cursor in
      Class.make_cursor  cursor_class cursor,
      Class.make_textbuf textbuf_class cursor
  end

  let with_cursor = Cursor.create
end

module Nav = struct

  type t = {
    mutable prev: page list;
    mutable page: page;
    mutable next: page list;

    title: cursor;
    body: cursor;
  }

  and page = string * (t -> unit)

  let null_page : page = "", ignore

  let not_closed t =
    if is_closed t.body then (
      if t.page != null_page then (
        t.prev <- [];
        t.next <- [];
        t.page <- null_page;
      );
      false
    ) else
      true

  let null = {
    prev  = [];
    page  = null_page;
    next  = [];

    title = null_cursor;
    body  = null_cursor;
  }

  let refresh t =
    if not_closed t then (
      clear t.title;
      text t.title (fst t.page);

      clear t.body;
      (snd t.page) t
    )

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
    if not_closed t then (
      link cursor "<<" (fun _ -> prev t);
      text cursor " ";
      link cursor "[?]" (fun _ -> refresh t);
      text cursor " ";
      link cursor ">>" (fun _ -> next t)
    )

  let make cursor label content =
    if not (is_closed cursor) then (
      let header = sub cursor in
      text cursor " ";
      let title = sub cursor in
      text cursor "\n\n";
      let body = sub cursor in
      let t = { prev = []; page = (label, content); next = []; title; body } in
      render_header t header;
      refresh t
    )

  let title t =
    t.title

  let body t =
    t.body

  let modal t label content =
    if not_closed t then (
      t.next <- [(label, content)];
      next t
    )
end

module Tree = struct

  type t = {
    indent: int;
    cursor: cursor;
  }

  let null = {
    indent = 0;
    cursor = null_cursor;
  }

  let not_closed t =
    not (is_closed t.cursor)

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

  let add_node children ?action ?(opened=ref false) t =
    indent t;
    let body = ref None in
    link t.cursor (if !opened then "[-]" else "[+]") (fun c ->
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
    let t' = { indent = t.indent + 1; cursor = sub t.cursor } in
    body := Some t';
    if !opened then children t';
    result

  let add ?children ?action ?opened t =
    if not_closed t then (
      match children with
      | None -> add_leaf ?action t
      | Some children -> add_node children ?action ?opened t
    ) else
      null_cursor

  let clear t = clear t.cursor
end
