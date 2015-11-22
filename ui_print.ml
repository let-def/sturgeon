open Sexp
open Session

type cursor = {
  action: action option;
  trope: cursor lazy_t Trope.t ref;
  editor: Ui_buf.editor;
  beginning: cursor lazy_t Trope.cursor;
  position: cursor lazy_t Trope.cursor;
}

and action = cursor -> unit

let closed =
  let _session, buffer = Ui_buf.create () in
  let editor = Ui_buf.edit buffer
      ~on_change:(fun _ ~start:_ ~old_len:_ ~text_raw:_ ~text_len:_ ~text:_ ~clickable:_ -> ())
      ~on_click:(fun _ _ -> ())
  in
  let rec cursor = lazy begin
    let trope, c = Trope.put_cursor (Trope.create ()) ~at:0 cursor in
    { action = None;
      trope = ref trope;
      editor;
      beginning = c;
      position = c;
    }
  end in
  Lazy.force cursor

let get_action c = (Lazy.force (Trope.content c)).action

let is_closed cursor = not (Trope.member !(cursor.trope) cursor.position)

let sub ?action current =
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

let text {action; editor; trope; beginning; position} ?(raw=false) ?properties text =
  (*let cmd = match properties with
    | None -> [S "text"; T text]
    | Some props ->
      let props = transform_list ~inj:void ~map:(fun x -> x) props in
      [S "text"; T text; S ":properties"; props]
  in*)
  if Trope.member !trope position then begin
    let start = Trope.position !trope position in
    let length = Ui_buf.string_length ~raw text in
    Ui_buf.change editor ~start ~old_len:0 ~text_raw:raw
      ~clickable:(action <> None) ~text;
    trope := Trope.insert_before !trope position length
  end

let clear {editor; trope; beginning; position} =
  if Trope.member !trope position then begin
    let start = Trope.position !trope beginning in
    let length = Trope.position !trope position - start in
    Ui_buf.change editor ~start ~old_len:length
      ~text_raw:true ~text:"" ~clickable:false;
    trope := Trope.remove_between !trope beginning position
  end

let link cursor ?raw ?properties msg action =
  let cursor = sub ~action:(Some action) cursor in
  text cursor ?raw ?properties msg

let printf (cursor : cursor) ?raw ?properties fmt =
  Printf.ksprintf (text cursor ?raw ?properties) fmt

let create_buffer () =
  let session, buffer = Ui_buf.create () in
  let trope = ref (Trope.create ()) in
  let on_change _ ~start ~old_len ~text_raw ~text_len ~text ~clickable =
    if old_len <> 0 then
      trope := Trope.remove ~at:start ~len:old_len !trope;
    if text_len <> 0 then
      trope := Trope.insert ~at:start ~len:text_len !trope
  and on_click _ offset =
    match Trope.find_before !trope offset with
    | None -> exit 4
    | Some cursor ->
      match get_action cursor with
      | None -> exit 5
      | Some action ->
        action (Lazy.force (Trope.content cursor))
  in
  let editor = Ui_buf.edit buffer ~on_change ~on_click in
  let rec cursor = lazy begin
    let t, beginning = Trope.put_cursor !trope ~at:0 cursor in
    let t, position  = Trope.put_after  t beginning cursor in
    trope := t;
    {editor; beginning; position; trope; action = None}
  end in
  let lazy cursor = cursor in
  session, cursor

let buffer_greetings name =
  let handler, cursor = create_buffer () in
  sexp_of_list [S "create-buffer"; T name; handler], cursor

let accept = function
  | M (Sink t) ->
    let handler, cursor = create_buffer () in
    t (Feed (C (S "accept", handler)));
    cursor, (fun str -> t (Feed (C (S "title", T str))))
  | _ -> invalid_arg "Ui_print.accept"
