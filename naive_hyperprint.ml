open Emacs_sexp
open Emacs_serge

type substitution = {
  start: int;
  length: int;
  replacement: string;
  action: bool;
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
      (if action then sym_t else sym_nil);
    ]
  in
  sink (Feed cmd)

let push_command buffer cmd =
  let opt = function
    | None -> "None"
    | Some _ -> "Some _"
  in
  Printf.eprintf
    "{ start = %d; length = %d; replacement = %S; action = %b } / sink = %s\n"
    cmd.start cmd.length cmd.replacement cmd.action (opt buffer.sink);
  if buffer.closed then ()
  else match buffer.sink with
    | None -> buffer.queue <- cmd :: buffer.queue
    | Some sink -> send_command sink cmd

let replace_sink commands sink =
  begin match commands.sink with
    | Some sink' -> sink' (Quit sym_nil)
    | None -> ()
  end;
  commands.sink <- None;
  match sink with
  | Some f ->
    let rec aux = function
      | [] -> ()
      | ls ->
        commands.queue <- [];
        List.iter (send_command f) (List.rev ls);
        aux commands.queue
    in
    aux commands.queue;
    commands.sink <- sink
  | None -> ()

type cursor = {
  action: action option;
  commands: command_stream;
  beginning: cursor lazy_t Naive_buf.cursor;
  position: cursor lazy_t Naive_buf.cursor;
}

and action = cursor -> unit

let get_action c = (Lazy.force (Naive_buf.content c)).action

let is_closed cursor = not (Naive_buf.valid cursor.position)

let sub ?action current =
  if is_closed current then current
  else
    let action = match action with
      | None -> get_action current.beginning
      | Some action -> action
    in
    let rec cursor = lazy begin
      let beginning = Naive_buf.before current.position cursor in
      let position = Naive_buf.after beginning cursor in
      { action; commands = current.commands; beginning; position }
    end in
    Lazy.force cursor


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
      action = (get_action cursor.beginning <> None) };
  Naive_buf.insert_before cursor.position (String.length text)

let clear cursor =
  let beginning = Naive_buf.position cursor.beginning in
  let position = Naive_buf.position cursor.position in
  push_command cursor.commands
    { start = beginning; length = position - beginning;
      replacement = ""; action = false };
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
  let buffer = Naive_buf.create () in
  let handler = M (Sink (function
    | Feed (C (S "sink", M (Sink sink))) ->
      Printf.eprintf "GOT SINK!\n";
      replace_sink commands (Some sink)
    | Feed (C (S "click", I point)) ->
      Printf.eprintf "GOT CLICK %d!\n" point;
      begin match Naive_buf.find_before buffer point with
        | None ->
          Printf.eprintf "NO CURSOR AT %d :(!\n" point;
        | Some cursor ->
          match get_action cursor with
          | None ->
            Printf.eprintf "NO ACTION AT %d :(!\n" point;
          | Some action ->
            action (Lazy.force (Naive_buf.content cursor))
      end;
    | Feed r -> cancel r
    | Quit (S "close") ->
      replace_sink commands None;
      commands.closed <- true;
      ()
    | Quit _ -> ()
    ))
  in
  let rec cursor = lazy begin
    let beginning = Naive_buf.put_cursor buffer ~at:0 cursor in
    let position = Naive_buf.after beginning cursor in
    {beginning; position; commands; action = None}
  end
  in
    endpoint.query (sexp_of_list
                    [S "create-buffer"; T name; handler]);
  Lazy.force cursor
