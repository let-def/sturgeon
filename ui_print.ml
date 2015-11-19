open Sexp
open Session

type cursor = {
  action: action option;
  trope: cursor lazy_t Trope.t ref;
  buffer: Ui_buf.t;
  beginning: cursor lazy_t Trope.cursor;
  position: cursor lazy_t Trope.cursor;
}

and action = cursor -> unit

let closed =
  let rec cursor = lazy begin
    let trope, c = Trope.put_cursor (Trope.create ()) ~at:0 cursor in
    { action = None;
      trope = ref trope;
      buffer = snd (Ui_buf.create ());
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
      let {buffer; trope; position} = current in
      let t, beginning = Trope.put_before !trope position  cursor in
      let t, position  = Trope.put_after  t beginning cursor in
      trope := t;
      {trope; buffer; action; beginning; position}
    end in
    Lazy.force cursor

let text {action; buffer; trope; beginning; position} ?(raw=false) ?properties text =
  (*let cmd = match properties with
    | None -> [S "text"; T text]
    | Some props ->
      let props = transform_list ~inj:void ~map:(fun x -> x) props in
      [S "text"; T text; S ":properties"; props]
  in*)
  if Trope.member !trope position then begin
    let start = Trope.position !trope position in
    let length = Ui_buf.string_length ~raw text in
    Ui_buf.local_change buffer ~start ~old_len:0 ~text_raw:raw
      ~clickable:(action <> None) ~text;
    trope := Trope.insert_before !trope position length
  end

let clear {buffer; trope; beginning; position} =
  if Trope.member !trope position then begin
    let start = Trope.position !trope beginning in
    let length = Trope.position !trope position - start in
    Ui_buf.local_change buffer ~start ~old_len:length
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
  let rec cursor = lazy begin
    let trope, beginning = Trope.put_cursor (Trope.create ()) ~at:0 cursor in
    let trope, position  = Trope.put_after  trope beginning cursor in
    {buffer; beginning; position; trope = ref trope; action = None}
  end in
  let lazy cursor = cursor in
  let trope = cursor.trope in
  let on_change ~start ~old_len ~text_raw ~text_len ~text ~clickable =
    if old_len <> 0 then
      trope := Trope.remove ~at:start ~len:old_len !trope;
    if text_len <> 0 then
      trope := Trope.insert ~at:start ~len:text_len !trope;
    `Keep

  and on_click offset =
    begin match Trope.find_before !trope offset with
    | None -> exit 4
    | Some cursor ->
      match get_action cursor with
      | None -> exit 5
      | Some action ->
        action (Lazy.force (Trope.content cursor))
    end;
    `Keep

  in
  Ui_buf.remote_changes buffer on_change;
  Ui_buf.remote_clicks buffer on_click;
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
