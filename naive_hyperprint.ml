open Emacs_sexp
open Emacs_serge

type substitution = {
  start: int;
  length: int;
  replacement: string;
  action: int option;
}

type command_stream = {
  mutable sink: t neg option;
  mutable closed: bool;
  mutable queue: substitution list;
}

let send_command sink {start; length; replacement; action} =
  let cmd = sexp_of_list [
      S "substitute";
      I start;
      I length;
      T replacement;
      (match action with None -> sym_nil | Some action -> I action);
    ]
  in
  sink (Feed cmd)

let push_command buffer cmd =
  if buffer.closed then ()
  else match buffer.sink with
    | None -> buffer.queue <- cmd :: buffer.queue
    | Some sink -> send_command sink cmd

type cursor = {
  commands: command_stream;
  action: action option;
  beginning: unit Naive_buf.cursor;
  position: unit Naive_buf.cursor;
}

and action = cursor -> unit

let is_closed cursor = not (Naive_buf.valid cursor.position)

let sub ?action current =
  if is_closed current then current
  else
    let action = match action with
      | None -> current.action
      | Some action -> action
    in
    let beginning = Naive_buf.before current.position () in
    let position = Naive_buf.after beginning () in
    { commands = current.commands; action; beginning; position }

let text cursor ?properties text =
  (*let cmd = match properties with
    | None -> [S "text"; T text]
    | Some props ->
      let props = transform_list ~inj:void ~map:(fun x -> x) props in
      [S "text"; T text; S ":properties"; props]
  in*)
  push_command cursor.commands
    { start = Naive_buf.position cursor.position;
      length = 0;
      replacement = text;
      action = None };
  Naive_buf.insert_before cursor.position (String.length text)

let clear cursor =
  let beginning = Naive_buf.position cursor.beginning in
  let position = Naive_buf.position cursor.position in
  push_command cursor.commands
    { start = beginning; length = position - beginning;
      replacement = ""; action = None };
  Naive_buf.remove_between cursor.beginning cursor.position

let link cursor ?properties msg action =
  let cursor = sub ~action:(Some action) cursor in
  text cursor ?properties msg

let printf (cursor : cursor) ?properties fmt =
  Printf.ksprintf (text cursor ?properties) fmt

let open_buffer endpoint name =
  let commands = {
    sink   = None;
    closed = false;
    queue  = [];
  } in
  let replace_sink buffer sink =
    begin match buffer.sink with
      | Some sink' -> sink' (Quit sym_nil)
      | None -> ()
    end;
    buffer.sink <- sink
  in
  let handler = M (Sink (function
    | Feed (C (S "sink", M (Sink sink))) ->
      replace_sink commands (Some sink)
    | Feed r -> cancel r
    | Quit _ ->
      replace_sink commands None;
      commands.closed <- true;
      ()
    ))
  in
  let buffer = Naive_buf.create () in
  let beginning = Naive_buf.put_cursor buffer ~at:0 () in
  let position = Naive_buf.after beginning () in
  let cursor = {beginning; position; commands; action = None} in
  endpoint.query (sexp_of_list
                    [S "create-buffer"; T name; handler]);
  cursor
