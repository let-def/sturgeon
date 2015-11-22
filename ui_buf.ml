open Sexp
open Session

type revision = {
  remote_revision: int;
  local_revision: int;
}

type substitute = {
  start: int;
  length: int;
  replacement: string;
  raw: bool;
  action: bool;
}

type command =
  | Substitute of substitute
  | Click of int

type command_stream = {
  mutable sink: t neg option;
  mutable closed: bool;
  mutable queue: (revision * command) list;
}

let sexp_of_revision {remote_revision; local_revision} =
  C (I remote_revision, I local_revision)

let cons_if cond x xs = if cond then x :: xs else xs

let send_command sink (rev,cmd) = match cmd with
  | Substitute {start; length; replacement; raw; action} ->
    let cmd = sexp_of_list (
        S "substitute" ::
        sexp_of_revision rev ::
        C (I start, I length) ::
        T replacement ::
        cons_if raw (S "raw") (cons_if action (S "action") [])
      )
    in
    sink (Feed cmd)
  | Click offset ->
    sink (Feed (sexp_of_list [
        S "click";
        sexp_of_revision rev;
        I offset
      ]))

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

type op =
  | R of int * int
  | I of int * int

type t = {
  commands: command_stream;
  mutable editors: editor list;
  mutable remote: int;
  mutable latest_remote: int;

  mutable local: int;
  mutable revisions: (int * op) list;
  mutable rev_tail: (int * op) list;
}

and editor = {
  buffer: t;
  on_change: (editor -> start:int -> old_len:int ->
              text_raw:bool -> text_len:int -> text:string ->
              clickable:bool -> unit);
  on_click: (editor -> int -> unit);
}

let revision_of_buffer { remote; local } =
  { local_revision = local; remote_revision = remote }

let push_command t cmd =
  let cmd = (revision_of_buffer t, cmd) in
  if t.commands.closed then ()
  else match t.commands.sink with
    | None -> t.commands.queue <- cmd :: t.commands.queue
    | Some sink ->
      t.latest_remote <- t.remote;
      send_command sink cmd

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
  end;
  if buffer.latest_remote < remote - 16 then
    let t = {start = 0; length = 0; raw = true;
             replacement = ""; action = false} in
    push_command buffer (Substitute t)

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

let create () =
  let commands = {
    sink   = None;
    closed = false;
    queue  = [];
  } in
  let buffer = { commands; editors = [];
                 remote = 0; latest_remote = 0; local = 0;
                 revisions = []; rev_tail = [] };
  in
  let handler = M (Sink (function
    | Feed (C (S "sink", M (Sink sink))) ->
      (*Printf.eprintf "GOT SINK!\n";*)
      replace_sink commands (Some sink)
    | Feed (C (S "click", C (C (I local, I remote), I point))) ->
      update_revisions buffer (local, remote);
      begin match commute_remote_op buffer commute_point point with
        | exception Not_found -> ()
        | point ->
          List.iter (fun editor -> editor.on_click editor point) buffer.editors;
      end
    | Feed (C (S "substitute",
               C (C (I local, I remote),
                  C (C (I start, I length),
                     C (T replacement, flags))))) ->
      update_revisions buffer (local, remote);
      begin match
        if length <> 0 then
          commute_remote_op buffer commute_remove (start, length)
        else
          commute_remote_op buffer commute_point start, 0
        with
        | exception Not_found -> ()
        | (start, length) ->
          let raw = sexp_mem (S "raw") flags in
          let clickable = sexp_mem (S "action") flags in
          let new_len = string_length ~raw replacement in
          let change editor =
            editor.on_change editor
              ~start ~old_len:length
              ~text_raw:raw ~text_len:new_len ~text:replacement
              ~clickable
          in
          List.iter change buffer.editors;
      end
    | Feed r ->
      cancel r
    | Quit (S "close") ->
      replace_sink commands None;
      commands.closed <- true;
      ()
    | Quit _ -> ()
    ))
  in
  handler, buffer

let cancel editor =
  editor.buffer.editors <-
    List.filter ((!=) editor) editor.buffer.editors

let edit buffer ~on_change ~on_click =
  let editor = { buffer; on_change; on_click } in
  buffer.editors <- editor :: buffer.editors;
  editor

let change editor ~start ~old_len ~text_raw ~text ~clickable =
  let t = editor.buffer in
  t.local <- t.local + 1;
  let text_len = string_length ~raw:text_raw text in
  if old_len <> 0 then
    t.revisions <- (t.local, R (start, old_len)) :: t.revisions;
  if text_len <> 0 then
    t.revisions <- (t.local, I (start, text_len)) :: t.revisions;
  push_command t
     (Substitute { start; length = old_len; raw = text_raw; replacement = text;
                   action = clickable });
  List.iter (fun editor' ->
      if editor != editor' then
        editor'.on_change editor'
          ~start ~old_len ~text_raw ~text_len ~text ~clickable
    ) t.editors

let click editor offset =
  let t = editor.buffer in
  push_command t (Click offset);
  List.iter (fun editor' ->
      if editor != editor' then
        editor'.on_click editor'
          offset
    ) t.editors
