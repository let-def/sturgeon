open Emacs_sexp
open Emacs_serge

type cursor = t neg

let text cursor ?properties text =
  let cmd = match properties with
    | None -> [S "text"; T text]
    | Some props ->
      let props = transform_list ~inj:void ~map:(fun x -> x) props in
      [S "text"; T text; S ":properties"; props]
  in
  cursor (Feed (sexp_of_list cmd))

let clear cursor =
  cursor (Feed (S "clear"))

let cancel_result = function
  | Feed x -> cancel x
  | _ -> ()

module Late = struct
  type cursor = {
    mutable cursor_buf: t result list;
    mutable cursor_fun: t neg;
  }

  let make () =
    let t = {
      cursor_buf = [];
      cursor_fun = (fun _ -> assert false)
    } in
    t.cursor_fun <- (fun msg -> t.cursor_buf <- msg :: t.cursor_buf);
    t

  let cursor q msg = q.cursor_fun msg

  let resume q f =
    q.cursor_fun <- f;
    let buf = List.rev q.cursor_buf in
    q.cursor_buf <- [];
    List.iter f buf

  let cancel q =
    resume q cancel_result
end

let open_cursor late = function
  | Feed (M (Sink cursor)) ->
    Late.resume late cursor
  | result ->
    Late.cancel late;
    cancel_result result

let sub ?action current =
  let late = Late.make () in
  let cursor = Late.cursor late in
  let cmd = match action with
    | None -> [S "sub"; M (Once (open_cursor late))]
    | Some f ->
      [S "sub"; M (Once (open_cursor late)); S ":action";
       M (Sink (function
           | Feed (S "t") -> f cursor
           | result -> cancel_result result
         ))
      ]
  in
  current (Feed (sexp_of_list cmd));
  cursor

let link cursor ?properties msg action =
  let cursor' = sub ~action cursor in
  text cursor' ?properties msg

let open_buffer endpoint name =
  let late = Late.make () in
  endpoint.query (sexp_of_list
                    [S "create-buffer"; T name; M (Once (open_cursor late))]);
  Late.cursor late

let printf (cursor : cursor) ?properties fmt =
  Printf.ksprintf (text cursor ?properties) fmt
