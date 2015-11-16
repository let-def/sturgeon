open Sexp
open Session

type command = {
  remote_revision: int;
  local_revision: int;
  start: int;
  length: int;
  replacement: string;
  utf8: bool;
  action: bool;
}

type command_stream = {
  mutable sink: t neg option;
  mutable closed: bool;
  mutable queue: command list;
}

let send_command sink
    {start; length; remote_revision; local_revision; replacement; utf8; action} =
  let cmd = sexp_of_list [
      S "substitute";
      C (I remote_revision, I local_revision);
      C (I start, I length);
      T replacement;
      (if utf8 then sym_t else sym_nil);
      (if action then sym_t else sym_nil);
    ]
  in
  sink (Feed cmd)

let push_command buffer cmd =
  (*let opt = function
    | None -> "None"
    | Some _ -> "Some _"
  in
  Printf.eprintf
    "{ start = %d; length = %d; replacement = %S; action = %b } / sink = %s\n"
    cmd.start cmd.length cmd.replacement cmd.action (opt buffer.sink);*)
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

type op =
  | R of int * int
  | I of int * int

type buffer = {
  mutable trope: cursor lazy_t Trope.t;
  mutable remote: int;

  mutable local: int;
  mutable revisions: (int * op) list;
  mutable rev_tail: (int * op) list;
}

and cursor = {
  action: action option;
  commands: command_stream;
  buffer: buffer;
  beginning: cursor lazy_t Trope.cursor;
  position: cursor lazy_t Trope.cursor;
}

and action = cursor -> unit

let closed =
  let rec cursor = lazy begin
    let trope, c = Trope.put_cursor (Trope.create ()) ~at:0 cursor in
    { action = None;
      commands = { sink = None; closed = true; queue = [] };
      buffer = { trope; remote = 0; local = 0;
                 revisions = []; rev_tail = [] };
      beginning = c;
      position = c;
    }
  end in
  Lazy.force cursor

let get_action c = (Lazy.force (Trope.content c)).action

let is_closed cursor = not (Trope.member cursor.buffer.trope cursor.position)

let sub ?action current =
  if is_closed current then current
  else
    let action = match action with
      | None -> get_action current.beginning
      | Some action -> action
    in
    let rec cursor = lazy begin
      let {buffer; commands; position} = current in
      let trope = buffer.trope in
      let trope, beginning = Trope.put_before trope position  cursor in
      let trope, position  = Trope.put_after  trope beginning cursor in
      buffer.trope <- trope;
      {action; buffer; commands; beginning; position}
    end in
    Lazy.force cursor

let string_length ?(utf8=false) str =
  if utf8 then
    let count = ref 0 in
    for i = 0 to String.length str - 1 do
      let c = Char.code str.[i] in
      if c land 0xC0 <> 0x80 then
        incr count
    done;
    !count
  else String.length str

let text {buffer; commands; beginning; position} ?(utf8=false) ?properties text =
  (*let cmd = match properties with
    | None -> [S "text"; T text]
    | Some props ->
      let props = transform_list ~inj:void ~map:(fun x -> x) props in
      [S "text"; T text; S ":properties"; props]
  in*)
  if Trope.member buffer.trope position then begin
    let start = Trope.position buffer.trope position in
    buffer.local <- buffer.local + 1;
    buffer.revisions <- (buffer.local, I (start, String.length text))
                       :: buffer.revisions;
    push_command commands
      { start; length = 0; utf8; replacement = text;
        remote_revision = buffer.remote;
        local_revision = buffer.local;
        action = (get_action beginning <> None) };
    buffer.trope <- Trope.insert_before buffer.trope position (String.length text)
  end

let clear {buffer; commands; beginning; position} =
  if Trope.member buffer.trope position then begin
    let start = Trope.position buffer.trope beginning in
    let length = Trope.position buffer.trope position - start in
    buffer.local <- buffer.local + 1;
    buffer.revisions <- (buffer.local, R (start, length)) :: buffer.revisions;
    push_command commands
      { start; length; utf8 = false;
        remote_revision = buffer.remote;
        local_revision = buffer.local;
        replacement = ""; action = false };
    buffer.trope <- Trope.remove_between buffer.trope beginning position
  end

let update_revisions buffer (local, remote) =
  buffer.remote <- remote;
  let rec filter = function
    | (local', _) :: xs when local' <= local -> filter xs
    | xs -> xs
  in
  let rec rev_filter acc = function
    | (local', _) as x :: xs when local' > local ->
      rev_filter (x :: acc) xs
    | _ -> acc
  in
  begin match filter buffer.rev_tail with
    | [] ->
      buffer.rev_tail <- rev_filter [] buffer.revisions;
      buffer.revisions <- []
    | xs ->
      buffer.rev_tail <- xs
  end

let commute_remove (_,op') (s2, l2) = match op' with
  | R (s1, l1) ->
    let remap x =
      if x < s1 then
        x
      else if x > s1 + l1 then
        x - l1
      else s1
    in
    let e2 = s2 + l2 in
    let s2 = remap s2 and e2 = remap e2 in
    if e2 = s2 then
      raise Not_found
    else (s2, e2 - s2)
  | I (s1, l1) ->
    if s1 < s2 then
      (s2 + l1, l2)
    else if s1 >= s2 + l2 then
      (s2, l2)
    else
      (s2, l2 + l1)

let commute_point (_,op') s2 = match op' with
  | R (s1, l1) ->
    if s2 < s1 then
      s2
    else if s2 >= s1 + l1 then
      (s2 - l1)
    else
      raise Not_found
  | I (s1, l1) ->
    if s1 <= s2 then
      (s2 + l1)
    else
      s2

let commute_remote_op buffer op_kind op_arg =
  let flip f x y = f y x in
  let op_arg = List.fold_left (flip op_kind) op_arg buffer.rev_tail in
  let op_arg = List.fold_right op_kind buffer.revisions op_arg in
  op_arg

let apply_remote_op buffer op =
  match
    op,
    match op with
    | R (s, l) -> commute_remote_op buffer commute_remove (s, l)
    | I (s, l) -> commute_remote_op buffer commute_point s, l
  with
  | exception Not_found -> ()
  | _, (_, 0) -> ()
  | R _, (at, len) -> buffer.trope <- Trope.remove buffer.trope ~at ~len
  | I _, (at, len) -> buffer.trope <- Trope.insert buffer.trope ~at ~len

let link cursor ?utf8 ?properties msg action =
  let cursor = sub ~action:(Some action) cursor in
  text cursor ?utf8 ?properties msg

let printf (cursor : cursor) ?utf8 ?properties fmt =
  Printf.ksprintf (text cursor ?utf8 ?properties) fmt

let create_buffer () =
  let commands = {
    sink   = None;
    closed = false;
    queue  = [];
  } in
  let buffer = { trope = Trope.create ();
                 remote = 0; local = 0; revisions = []; rev_tail = [] };
  in
  let handler = M (Sink (function
    | Feed (C (S "sink", M (Sink sink))) ->
      (*Printf.eprintf "GOT SINK!\n";*)
      replace_sink commands (Some sink)
    | Feed (C (S "click", C (C (I local, I remote), I point))) ->
      (*Printf.eprintf "GOT CLICK %d!\n" point;*)
      update_revisions buffer (local, remote);
      begin match commute_remote_op buffer commute_point point with
        | exception Not_found -> ()
        | point ->
          match Trope.find_before buffer.trope point with
          | None ->
            (*Printf.eprintf "NO CURSOR AT %d :(!\n" point;*)
            ()
          | Some cursor ->
            match get_action cursor with
            | None ->
              (*Printf.eprintf "NO ACTION AT %d :(!\n" point;*)
              ()
            | Some action ->
              action (Lazy.force (Trope.content cursor))
      end
    | Feed (C (S "substitute",
               C (C (I local, I remote),
                  C (C (I start, I length),
                     C (T replacement, C (utf8, C (S "nil", S "nil"))))))) ->
      update_revisions buffer (local, remote);
      if length <> 0 then
        apply_remote_op buffer (R (start, length));
      let utf8 = utf8 <> sym_nil in
      if replacement <> "" then
        apply_remote_op buffer (I (start, string_length ~utf8 replacement))
    | Feed r -> cancel r
    | Quit (S "close") ->
      replace_sink commands None;
      commands.closed <- true;
      ()
    | Quit _ -> ()
    ))
  in
  let rec cursor = lazy begin
    let trope = buffer.trope in
    let trope, beginning = Trope.put_cursor trope ~at:0 cursor in
    let trope, position  = Trope.put_after  trope beginning cursor in
    buffer.trope <- trope;
    {buffer; beginning; position; commands; action = None}
  end
  in
  handler, Lazy.force cursor

let buffer_greetings name =
  let handler, cursor = create_buffer () in
  sexp_of_list [S "create-buffer"; T name; handler], cursor

let accept = function
  | M (Sink t) ->
    let handler, cursor = create_buffer () in
    t (Feed (C (S "accept", handler)));
    cursor, (fun str -> t (Feed (C (S "title", T str))))
  | _ -> invalid_arg "Ui_print.accept"
