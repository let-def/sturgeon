open Ui_print

type t = {
  mutable prev: page list;
  mutable page: page;
  mutable next: page list;

  title: cursor;
  body: cursor;
}

and page = string * (t -> title:cursor -> body:cursor -> unit)

let refresh t =
  clear t.title;
  text t.title (fst t.page);

  clear t.body;
  (snd t.page) t ~title:t.title ~body:t.body

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
  link cursor "<<" (fun _ -> prev t);
  text cursor " ";
  link cursor "[?]" (fun _ -> refresh t);
  text cursor " ";
  link cursor ">>" (fun _ -> next t)

let make cursor label content =
  let header = sub cursor in
  text cursor " ";
  let title = sub cursor in
  text cursor "\n\n";
  let body = sub cursor in
  let t = { prev = []; page = (label, content); next = []; title; body } in
  render_header t header;
  refresh t

let modal t label content =
  t.next <- [(label, content)];
  next t
