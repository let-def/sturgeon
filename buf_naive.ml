module O = Order_indir

type 'a t = {
  mutable cursors: (int * 'a cursor) list;
  on_invalidate: 'a cursor -> unit;
  root: O.t;
}

and 'a cursor = {
  buffer: 'a t;
  content: 'a;
  position: O.t;
}

let create ?(on_invalidate=ignore) () = {
  cursors = [];
  on_invalidate;
  root = O.root ();
}

let buffer c = c.buffer
let content c = c.content

let valid c = O.is_valid c.position

let forget c =
  c.buffer.on_invalidate c;
  O.forget c.position

let clear t =
  List.iter (fun (_,c) -> forget c) t.cursors;
  t.cursors <- []

let is_empty t = t.cursors = []

let compare a b =
  assert (valid a && valid b && a.buffer == b.buffer);
  let r =
    if a == b then
      0
    else
      let rec aux = function
        | [] -> assert false
        | (_,x) :: _ when x == a -> -1
        | (_,x) :: _ when x == b -> 1
        | _ :: xs -> aux xs
      in
      aux a.buffer.cursors
  in
  assert (r = O.compare a.position b.position);
  r

let position c =
  assert (valid c);
  let rec aux n = function
    | [] -> assert false
    | (n',c') :: xs ->
      let n = n + n' in
      if c == c' then n
      else aux n xs
  in
  aux 0 c.buffer.cursors

let remove t ~at ~len =
  assert (at >= 0);
  assert (len >= 0);
  let rec skip_right len = function
    | [] -> []
    | (n, c) :: xs when len <= n ->
      (n - len, c) :: xs
    | (n, c) :: xs ->
      forget c;
      skip_right (len - n) xs
  in
  let rec skip_left at = function
    | [] -> []
    | (n,c) :: xs when at < n ->
      skip_right len ((n - at, c) :: xs)
    | (n,_ as cell) :: xs ->
      cell :: skip_left (at - n) xs
  in
  t.cursors <- skip_left at t.cursors

let insert t ~at ~len =
  assert (at >= 0);
  assert (len >= 0);
  let rec shift at = function
    | [] -> []
    | (n,c) :: xs when at < n ->
      (n + len, c) :: xs
    | (n,_ as cell) :: xs ->
      cell :: shift (at - n) xs
  in
  t.cursors <- shift at t.cursors

let remove_between c1 c2 =
  assert (valid c1 && valid c2);
  if c1 == c2 then ()
  else begin
    assert (compare c1 c2 < 0);
    let rec drop = function
      | [] -> assert false
      | (_, c) :: xs when c == c2 ->
        (0, c) :: xs
      | (_, c) :: xs ->
        forget c;
        drop xs
    in
    let rec skip = function
      | [] -> assert false
      | (n, c) :: xs when c == c1 ->
        (n, c) :: drop xs
      | x :: xs ->
        x :: skip xs
    in
    c1.buffer.cursors <- skip c1.buffer.cursors
  end


let remove_before c len =
  assert (valid c);
  assert (len >= 0);
  let pos = position c in
  let at, len =
    if pos < len then
      0, pos
    else
      pos - len, len
  in
  remove c.buffer ~at ~len;
  let rec clean = function
    | [] -> assert false
    | (n, c') :: xs ->
      assert (n = 0);
      if c == c' then
        xs
      else
        (forget c; clean xs)
  in
  let rec cleanup pos = function
    | [] -> assert false
    | ((n,c') :: xs) as tail when n = pos ->
      if c == c' then
        tail
      else (forget c'; (n, c) :: clean xs)
    | (n, _ as cell) :: xs ->
      assert (pos > n);
      cell :: cleanup (pos - n) xs
  in
  c.buffer.cursors <- cleanup at c.buffer.cursors

let remove_after c len =
  assert (valid c);
  assert (len >= 0);
  let rec drop pos = function
    | [] -> []
    | (n, c') :: xs when pos <= n ->
      (n - pos, c') :: xs
    | (n, c') :: xs ->
      forget c';
      drop (pos - n) xs
  in
  let rec shift = function
    | [] -> assert false
    | (_,c' as cell) :: xs when c == c' ->
      cell :: drop len xs
    | cell :: xs -> cell :: shift xs
  in
  c.buffer.cursors <- shift c.buffer.cursors

let insert_before c len =
  assert (valid c);
  assert (len >= 0);
  let rec shift = function
    | [] -> assert false
    | (n,c') :: xs when c == c' ->
      (n + len, c) :: xs
    | cell :: xs -> cell :: shift xs
  in
  c.buffer.cursors <- shift c.buffer.cursors

let insert_after c len =
  assert (valid c);
  assert (len >= 0);
  let rec shift = function
    | [] -> assert false
    | (_,c' as cell) :: xs when c == c' ->
      cell :: (match xs with
          | [] -> []
          | (n, c'') :: xs' -> (n + len, c'') :: xs'
        )
    | cell :: xs -> cell :: shift xs
  in
  c.buffer.cursors <- shift c.buffer.cursors

let put_cursor t ~at content =
  assert (at >= 0);
  let rcursor = ref None in
  let rec aux p at = function
    | [] ->
      let cursor = { buffer = t; position = O.after p; content } in
      rcursor := Some cursor;
      [(at, cursor)]
    | (n, c) :: xs when at < n ->
      let cursor = { buffer = t; position = O.after p; content } in
      rcursor := Some cursor;
      (at, cursor) :: (n - at, c) :: xs
    | (n, c as cell) :: xs ->
      cell :: aux c.position (at - n) xs
  in
  t.cursors <- aux t.root at t.cursors;
  match !rcursor with
  | None -> assert false
  | Some c -> c

let rem_cursor c0 =
  let rec aux = function
    | [] -> assert false
    | [(_,c)] when c == c0 -> []
    | (n,c) :: (n',c') :: xs when c == c0 ->
      (n + n', c') :: xs
    | x :: xs ->
      x :: aux xs
  in
  forget c0;
  c0.buffer.cursors <- aux c0.buffer.cursors

let before ({buffer; position} as c0) content =
  assert (valid c0);
  let c = {buffer; position = O.before c0.position; content} in
  let rec aux = function
    | [] -> assert false
    | (n, c0') :: xs when c0 == c0' ->
      (n, c) :: (0, c0) :: xs
    | x :: xs ->
      x :: aux xs
  in
  c0.buffer.cursors <- aux c0.buffer.cursors;
  c

let after ({buffer; position} as c0) content =
  assert (valid c0);
  let c = {buffer; position = O.after c0.position; content} in
  let rec aux = function
    | [] -> assert false
    | (_, c0' as cell) :: xs when c0 == c0' ->
      cell :: (0, c) :: xs
    | x :: xs ->
      x :: aux xs
  in
  c0.buffer.cursors <- aux c0.buffer.cursors;
  c

let find_before t n =
  let rec aux n = function
    | [] -> None
    | (n', _) :: _ when n < n' -> None
    | (n', c) :: (n'', _) :: _ when n >= n' && n - n' <= n'' ->
      Some c
    | (n', _) :: xs ->
      aux (n - n') xs
  in
  aux n t.cursors

let find_after t n =
  let rec aux n = function
    | [] -> None
    | (n', c) :: _ when n < n' -> Some c
    | (n', _) :: xs ->
      aux (n - n') xs
  in
  aux n t.cursors

let cursor_before c =
  assert (valid c);
  let rec aux = function
    | [] -> assert false
    | (_, c') :: (_, c0) :: _ when c == c0 -> Some c'
    | _ :: xs -> aux xs
  in
  match c.buffer.cursors with
  | (_, c') :: _ when c == c' -> None
  | l -> aux l

let cursor_after c =
  assert (valid c);
  let rec aux = function
    | [] -> assert false
    | [(_, c0)] when c == c0 -> None
    | (_, c0) :: (_, c') :: _ when c == c0 -> Some c'
    | _ :: xs -> aux xs
  in
  aux c.buffer.cursors
