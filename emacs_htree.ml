open Emacs_hyperprint

type t = {
  indent: int;
  cursor: cursor;
}

let make cursor =
  { indent = 0; cursor = Emacs_hyperprint.sub cursor }

let indent t =
  if t.indent > 0 then
    text t.cursor (String.make t.indent ' ')

let add_leaf ?action t =
  indent t;
  text t.cursor "[ ] ";
  let result = sub ?action t.cursor in
  text t.cursor "\n";
  result

let add_node children ?action t =
  indent t;
  let body = ref None in
  let opened = ref false in
  link t.cursor "[+]" (fun c ->
      match !body with
      | None -> ()
      | Some t' when !opened ->
        opened := false;
        clear c; text c "[+]";
        clear t'.cursor
      | Some t' ->
        opened := true;
        clear c; text c "[-]";
        children t'
    );
  text t.cursor " ";
  let result = sub ?action t.cursor in
  text t.cursor "\n";
  body := Some { indent = t.indent + 1; cursor = sub t.cursor };
  result

let add ?children ?action t =
  match children with
  | None -> add_leaf ?action t
  | Some children -> add_node children ?action t

let clear t = clear t.cursor

