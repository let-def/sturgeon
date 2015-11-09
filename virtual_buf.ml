module O = Order_indir

module T = Mbt.Make (struct
    type 'a measurable = 'a * int
    type measure = int
    let empty = 0
    let cat a b c = a + (snd b) + c
  end)

type 'a t = {
  root: O.t;
  mutable tree: 'a cursor T.t;
  on_invalidate: 'a cursor -> unit;
}

and 'a cursor = {
  buffer: 'a t;
  content: 'a;
  position: O.t;
}

let create ?(on_invalidate=ignore) () = {
  root = O.root ();
  tree = T.leaf;
  on_invalidate;
}

let buffer c = c.buffer
let content c = c.content

let valid c = O.is_valid c.position
let forget c =
  c.buffer.on_invalidate c;
  O.forget c.position

let compare a b =
  O.compare a.position b.position

let clear t =
  let rec aux = function
    | T.Leaf -> ()
    | T.Node (_, l, (c, _), r, _) ->
      aux l;
      forget c;
      aux r
  in
  aux t.tree;
  t.tree <- T.leaf

let is_leaf = function
  | T.Leaf -> true
  | T.Node _ -> false

let is_empty t = is_leaf t.tree

let position c0 =
  let rec traverse n = function
    | T.Leaf -> raise Not_found
    | T.Node (_, l, (c, offset), r, _) ->
      let o = compare c0 c in
      if o < 0 then
        traverse n l
      else
        let n = n + T.measure l + offset in
        if o > 0 then
          traverse n r
        else
          n
  in
  traverse 0 c0.buffer.tree

let rec shift n = function
  | T.Leaf -> T.leaf
  | T.Node (_, T.Leaf, (c, offset), r, _) ->
    T.node T.leaf (c, offset + n) r
  | T.Node (_, l, cell, r, _) ->
    T.node (shift n l) cell r

let shift n tree = if n = 0 then tree else shift n tree

let rem_cursor c0 =
  let rec traverse = function
    | T.Leaf -> raise Not_found
    | T.Node (_, l, (c, offset as cell), r, _) ->
      let o = compare c0 c in
      if o < 0 then
        T.node (traverse l) cell r
      else if o > 0 then
        T.node l cell (traverse r)
      else begin
        T.join l (shift offset r)
      end
  in
  let tree = traverse c0.buffer.tree in
  forget c0;
  c0.buffer.tree <- tree

let put_cursor t ~at content =
  let cursor = ref None in
  let rec traverse before at = function
    | T.Leaf ->
      let c = { buffer = t; position = O.after before; content} in
      cursor := Some c;
      T.node T.leaf (c, at) T.leaf
    | T.Node (_, l, (c, offset as cell), r, _) ->
      let pos = T.measure l + offset in
      if at < pos then
        let l' = traverse before at l in
        T.node l' (c, pos - T.measure l') r
      else
        T.node l cell (traverse c.position (at - pos) r)
  in
  t.tree <- traverse t.root at t.tree;
  match !cursor with
  | None -> assert false
  | Some x -> x

let rec forget_tree = function
  | T.Leaf -> ()
  | T.Node (_, l, (c, _), r, _) ->
    forget c;
    forget_tree l;
    forget_tree r

let insert t ~at ~len =
  let rec aux n = function
    | T.Leaf -> assert false
    | T.Node (_, l, (c, n1 as cell), r, _) ->
      let n0 = T.measure l in
      if n < n0 then
        ((* l cannot be leaf, otherwise n0 = T.measure l = would be 0.
            But n > 0, so n < n0 is impossible. *)
          assert (not (is_leaf l));
          T.node (aux n l) cell r)
      else if n - n0 < n1 then
        T.node l (c, len + n1) r
      else
        T.node l cell (aux (n - n0 - n1) r)
  in
  assert (at >= 0 && len >= 0);
  t.tree <- aux at t.tree

let remove t ~at ~len =
  assert (at >= 0 && len >= 0);
  let len = ref len in
  let rec aux n = function
    | T.Leaf -> assert false
    | T.Node (_, l, (c, n1 as cell), r, _) ->
      let n0 = T.measure l in
      let starts_here = n < n0 || n = (-1) in
      let l =
        if starts_here then
          ((* l cannot be leaf, otherwise n0 = T.measure l = would be 0.
              But n > 0, so n < n0 is impossible. *)
            assert (not (is_leaf l));
            aux n l)
          else
            l
      and n = if starts_here then 0 else n - n0
      in
      let len' = !len in
      if len' > 0 then begin
        (* We still have to remove things *)
        if starts_here || n < n1 then
          (* At the left of the current node ... *)
          if n + len' <= n1 then
            (* But less than the current node. *)
            (len := 0;
             T.node l (c, n1 - len') r)
          else
            (* Including the current node *)
            (len := len' - n1;
             forget c;
             T.join l (aux (-1) r))
        else
          (* At the right of the current node *)
          T.node l cell (aux (n - n1) r)
      end
      else
        T.node l cell r
  in
  t.tree <- aux at t.tree

let remove_between c1 c2 =
  assert (valid c1 && valid c2);
  if c1 == c2 then ()
  else begin
    assert (compare c1 c2 < 0);
    let rec cut_left = function
      | T.Leaf -> assert false
      | T.Node (_, l, (c, _ as cell), r, _) ->
        let c1 = compare c1 c in
        if c1 < 0 then
          (forget c; forget_tree r;
           cut_left l)
        else if c1 > 0 then
          T.node l cell (cut_left r)
        else (* c1 = 0 *)
          (forget_tree r;
           T.node l cell T.leaf)
    in
    let rec cut_right = function
      | T.Leaf -> assert false
      | T.Node (_, l, (c, _ as cell), r, _) ->
        let c2 = compare c2 c in
        if c2 > 0 then
          (forget c; forget_tree l;
           cut_right r)
        else if c2 < 0 then
          T.node (cut_right l) cell r
        else (* c2 = 0 *)
          (forget_tree l;
           T.node T.leaf cell r)
    in
    let rec aux = function
      | T.Leaf -> assert false
      | T.Node (_, l, (c, _ as cell), r, _) ->
        let c1 = compare c1 c and c2 = compare c2 c in
        if c2 < 0 then
          T.node (aux l) cell r
        else if c1 > 0 then
          T.node l cell (aux r)
        else if c1 = 0 then
          T.node l cell (cut_right r)
        else if c2 = 0 then
          T.node (cut_left l) (c, 0) r
        else (* c1 < 0 && c2 > 0 *)
          (forget c; T.join (cut_left l) (cut_right r))
    in
    c1.buffer.tree <- aux c1.buffer.tree
  end

let remove_after c len =
  assert (valid c && len >= 0);
  if len > 0 then begin
    let len = ref len in
    let rec rem = function
      | T.Leaf -> T.leaf
      | T.Node (_, l, (c, n1 as cell), r, _) ->
        let n0 = T.measure l in
        if n0 > !len then
          let l = rem l in
          assert (!len = 0);
          T.node l cell r
        else begin
          len := !len - n0;
          forget_tree l;
          if !len <= n1 then begin
            let result = T.node T.leaf (c, n1 - !len) r in
            len := 0;
            result
          end else begin
            len := !len - n1;
            forget c;
            rem r
          end
        end
    in
    let rec seek = function
      | T.Leaf -> assert false
      | T.Node (_, l, (c', n as cell), r, _) ->
        let c = compare c c' in
        if c = 0 then
          T.node l cell (rem r)
        else if c < 0 then
          let l = seek l in
          let len' = !len in
          if len' = 0 then
            T.node l cell r
          else if len' <= n then
            (len := 0;
             T.node l (c', n - len') r)
          else
            (len := len' - n;
             forget c';
             T.join l (rem r))
        else (* c > 0 *)
          T.node l cell (seek r)
    in
    c.buffer.tree <- seek c.buffer.tree
  end

let remove_before c len =
  assert (valid c && len >= 0);
  if len > 0 then begin
    let len = ref len in
    let rec rem = function
      | T.Leaf -> T.leaf
      | T.Node (_, l, (c', n1 as cell), r, _) ->
        let n0 = T.measure r in
        if n0 > !len then
          let r = rem r in
          assert (!len <= 0);
          T.node l cell r
        else begin
          len := !len - n0;
          forget_tree r;
          let len' = !len in
          if len' <= n1 then begin
            forget c';
            len := - (n1 - !len);
            l
          end else begin
            len := !len - n1;
            forget c';
            rem l
          end
        end
    in
    let rec seek = function
      | T.Leaf -> assert false
      | T.Node (_, l, (c', n as cell), r, _) ->
        let c = compare c c' in
        if c = 0 then
          let l = rem l in
          let len' = !len in
          if len' >= 0 then
            T.node l cell r
          else (
            len := 0;
            T.node l (c', n - len') r
          )
        else if c > 0 then
          let r = seek r in
          let len' = !len in
          if len' = 0 then
            T.node l cell r
          else if len' <= n then
            (len := 0;
             forget c';
             T.join l (shift (n - len') r))
          else
            (len := len' - n;
             forget c';
             let l = rem l in
             let len' = !len in
             if len' < 0 && not (is_leaf r) then
               (len := 0;
                T.join (rem l) (shift (-len') r))
             else
               T.join (rem l) r
            )
        else (* c < 0 *)
          let l = seek l in
          let len' = !len in
          if len' < 0 then
            (len := 0;
             T.node l (c', n - len') r)
          else
            T.node l cell r
    in
    c.buffer.tree <- seek c.buffer.tree
  end

let insert_before c len =
  assert (valid c && len >= 0);
  if len > 0 then begin
    let rec aux = function
      | T.Leaf -> assert false
      | T.Node (_, l, (c', n as cell), r, _) ->
        let c = compare c c' in
        if c < 0 then
          T.node (aux l) cell r
        else if c > 0 then
          T.node l cell (aux r)
        else
          T.node l (c', n + len) r
    in
    c.buffer.tree <- aux c.buffer.tree
  end

let insert_after c len =
  assert (valid c && len >= 0);
  if len > 0 then begin
    let len = ref len in
    let rec aux = function
      | T.Leaf -> assert false
      | T.Node (_, l, (c', n as cell), r, _) as tree ->
        let c = compare c c' in
        if c < 0 then
          let l = aux l in
          if !len > 0 then
            let result = T.node l (c', n + !len) r in
            len := 0;
            result
          else
            T.node l cell r
        else if c > 0 then
          T.node l cell (aux r)
        else if is_leaf r then
          tree
        else (
          let result = T.node l cell (shift !len r) in
          len := 0;
          result
        )
    in
    c.buffer.tree <- aux c.buffer.tree
  end


let before c0 content =
  assert (valid c0);
  let c = {buffer = c0.buffer; position = O.before c0.position; content} in
  let rec traverse = function
    | T.Leaf -> assert false
    | T.Node (_, l, (c', offset as cell), r, _) ->
      let i = compare c0 c' in
      if i = 0 then
        T.node (T.node l (c, offset) T.leaf) (c', 0) r
      else if i < 0 then
        T.node (traverse l) cell r
      else
        T.node l cell (traverse r)
  in
  c0.buffer.tree <- traverse c0.buffer.tree;
  c

let after c0 content =
  assert (valid c0);
  let c = {buffer = c0.buffer; position = O.after c0.position; content} in
  let rec traverse = function
    | T.Leaf -> assert false
    | T.Node (_, l, (c', _ as cell), r, _) ->
      let i = compare c0 c' in
      if i = 0 then
        T.node (T.node l cell T.leaf) (c, 0) r
      else if i < 0 then
        T.node (traverse l) cell r
      else
        T.node l cell (traverse r)
  in
  c0.buffer.tree <- traverse c0.buffer.tree;
  c

let find_before t n =
  let rec aux n = function
    | T.Leaf -> None
    | T.Node (_, l, (c, n'), r, _) ->
      let n' = T.measure l + n' in
      if n < n' then
        aux n l
      else if n = n' then
        Some c
      else match aux (n - n') r with
        | Some _ as result -> result
        | None -> Some c
  in
  aux n t.tree

let find_after t n =
  let rec aux n = function
    | T.Leaf -> None
    | T.Node (_, l, (c, n1), r, _) ->
      let n0 = T.measure l in
      if n <= n0 then
        let result = aux n l in
        assert (result <> None);
        result
      else if n <= n0 + n1 then
        Some c
      else
        aux (n - n0 - n1) r
  in
  aux n t.tree

let cursor_before c =
  assert (valid c);
  let rec aux = function
    | T.Leaf -> None
    | T.Node (_, l, (c', n1), r, _) ->
      let c = compare c c' in
      if c <= 0 then
        aux l
      else match aux r with
        | Some _ as result -> result
        | None -> Some c'
  in
  aux c.buffer.tree

let cursor_after c =
  assert (valid c);
  let rec aux = function
    | T.Leaf -> None
    | T.Node (_, l, (c', n1), r, _) ->
      let c = compare c c' in
      if c >= 0 then
        aux r
      else match aux l with
        | Some _ as result -> result
        | None -> Some c'
  in
  aux c.buffer.tree
