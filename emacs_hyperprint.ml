open Emacs_sexp
open Emacs_serge

type cursor = {
  mutable closed: bool;
  mutable buff: t result list;
  mutable sink: t neg;
}

let make_cursor () =
  let t = {
    closed = false;
    buff = [];
    sink = (fun _ -> assert false);
  } in
  t.sink <- (fun msg -> t.buff <- msg :: t.buff);
  t

let resume q f =
  q.sink <- f;
  let buff = List.rev q.buff in
  q.buff <- [];
  List.iter f buff

let cancel_result = function
  | Feed x -> cancel x
  | _ -> ()

let cancel_cursor q =
  q.closed <- true;
  q.sink cancel_message;
  resume q cancel_result

let sink_of_cursor q =
  M (Sink (fun x -> q.sink x))

let open_cursor cursor =
  M (Sink (function
      | Feed (M (Sink sink)) -> resume cursor sink
      | Quit _ -> cancel_cursor cursor
      | result -> cancel_result result))

let sub ?action current =
  let cursor = make_cursor () in
  let cmd = match action with
    | None -> [S "sub"; open_cursor cursor]
    | Some f ->
      [S "sub"; open_cursor cursor; S ":action";
       M (Sink (function
           | Feed (S "t") -> f cursor
           | Quit _ -> cancel_cursor cursor
           | result -> cancel_result result
         ))
      ]
  in
  current.sink (Feed (sexp_of_list cmd));
  cursor

let text cursor ?properties text =
  let cmd = match properties with
    | None -> [S "text"; T text]
    | Some props ->
      let props = transform_list ~inj:void ~map:(fun x -> x) props in
      [S "text"; T text; S ":properties"; props]
  in
  cursor.sink (Feed (sexp_of_list cmd))

let is_closed cursor = cursor.closed

let clear cursor =
  cursor.sink (Feed (S "clear"))

let link cursor ?properties msg action =
  let cursor = sub ~action cursor in
  text cursor ?properties msg

let printf (cursor : cursor) ?properties fmt =
  Printf.ksprintf (text cursor ?properties) fmt

let open_buffer endpoint name =
  let cursor = make_cursor () in
  endpoint.query (sexp_of_list
                    [S "create-buffer"; T name; open_cursor cursor]);
  cursor
