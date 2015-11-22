open Sexp
open Session

module Class = struct
  type 'a t = {
    text      : 'a -> ?raw:bool -> ?properties:basic -> string -> unit;
    clear     : 'a -> unit;
    sub       : ?action:action option -> 'a -> cursor;
    is_closed : 'a -> bool;
  }

  and cursor = Cursor : 'a t * 'a -> cursor

  and action = cursor -> unit

  let make c i = Cursor (c, i)
end

type cursor = Class.cursor
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

module Prim = struct

  type t = {
    action: action option;
    trope: t lazy_t Trope.t ref;
    mutable editor: Ui_buf.buffer;
    beginning: t lazy_t Trope.cursor;
    position: t lazy_t Trope.cursor;
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
        let {editor; trope; position} = current in
        let t, beginning = Trope.put_before !trope position  cursor in
        let t, position  = Trope.put_after  t beginning cursor in
        trope := t;
        {trope; editor; action; beginning; position}
      end in
      Lazy.force cursor

  let text {action; editor; trope; beginning; position} ?raw ?properties text =
    (*let cmd = match properties with
      | None -> [S "text"; T text]
      | Some props ->
        let props = transform_list ~inj:void ~map:(fun x -> x) props in
        [S "text"; T text; S ":properties"; props]
      in*)
    if Trope.member !trope position then begin
      let start = Trope.position !trope position in
      let length = Ui_buf.string_length ?raw text in
      Ui_buf.change editor start 0 ?raw ~clickable:(action <> None) text;
      trope := Trope.insert_before !trope position length
    end

  let clear {editor; trope; beginning; position} =
    if Trope.member !trope position then begin
      let start = Trope.position !trope beginning in
      let length = Trope.position !trope position - start in
      Ui_buf.change editor start length "" ~raw:true ~clickable:false;
      trope := Trope.remove_between !trope beginning position
    end

  let rec cursor_class = {Class. text; sub; clear; is_closed}
  and sub ?action current =
    Class.Cursor (cursor_class, sub' ?action current)

  let buffer_class = {
    Ui_buf.Class.
    connect = (fun t buffer -> t.editor <- buffer);
    connected = ignore;
    change = (fun t text ->
        let open Ui_buf in
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

  let create_buffer () =
    let session, editor = Ui_buf.session () in
    let trope = ref (Trope.create ()) in
    let rec cursor = lazy begin
      let t, beginning = Trope.put_cursor !trope ~at:0 cursor in
      let t, position  = Trope.put_after  t beginning cursor in
      trope := t;
      {editor; beginning; position; trope; action = None}
    end in
    let lazy cursor = cursor in
    session, (Class.Cursor (cursor_class, cursor))
end

let buffer_greetings ~name =
  let handler, cursor = Prim.create_buffer () in
  sexp_of_list [S "create-buffer"; T name; handler], cursor

let accept = function
  | M (Sink t) ->
    let handler, cursor = Prim.create_buffer () in
    t (Feed (C (S "accept", handler)));
    cursor, (fun str -> t (Feed (C (S "title", T str))))
  | _ -> invalid_arg "Ui_print.accept"

let rec closed = Class.Cursor ({
    Class.
    text      = (fun () ?raw:_ ?properties:_ _ -> ());
    clear     = (fun () -> ());
    sub       = (fun ?action:_ () -> closed);
    is_closed = (fun () -> true);
  }, ())
