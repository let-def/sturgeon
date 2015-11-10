module O = Order_managed

module T = Mbt.Make (struct
    type 'a measurable = 'a * int
    type measure = int
    let empty = 0
    let cat a b c = a + (snd b) + c
  end)

type 'a t = {
  root: O.t;
  tree: 'a cursor T.t;
}

and 'a cursor = {
  content: 'a;
  position: O.t;
}

let create () = {
  root = O.root ();
  tree = T.leaf;
}

let content c = c.content

let validate t c msg =
  if not (O.same_order t.root c.position) then
    invalid_arg msg

let update t f =
  {t with tree = f t.tree}

let clear t = {t with tree = T.leaf}

let compare a b =
  O.compare a.position b.position

let is_leaf = function
  | T.Leaf -> true
  | T.Node _ -> false

let is_empty t = is_leaf t.tree

let member t c =
  let rec aux = function
  | T.Leaf -> false
  | T.Node (_, l, (c', _), r, _) ->
    let o = compare c c' in
    if o = 0 then
      true
    else if o < 0 then
      aux l
    else
      aux r
  in
  aux t.tree

let position t c0 =
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
  traverse 0 t.tree

let rec shift n = function
  | T.Leaf -> T.leaf
  | T.Node (_, T.Leaf, (c, offset), r, _) ->
    T.node T.leaf (c, offset + n) r
  | T.Node (_, l, cell, r, _) ->
    T.node (shift n l) cell r

let shift n tree = if n = 0 then tree else shift n tree

let rem_cursor t c0 =
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
  update t traverse

let put_cursor t ~at content =
  if at < 0 then
    invalid_arg "Buf.put_cursor: [at] must be >= 0";
  let cursor = ref None in
  let rec traverse before at = function
    | T.Leaf ->
      let c = {position = O.after before; content} in
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
  let t = update t (traverse t.root at) in
  match !cursor with
  | None -> assert false
  | Some x -> t, x

let insert t ~at ~len =
  if at < 0 then
    invalid_arg "Buf.insert: [at] must be >= 0";
  if len < 0 then
    invalid_arg "Buf.insert: [len] must be >= 0";
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
  if is_leaf t.tree then t
  else update t (aux at)

let remove t ~at ~len =
  if at < 0 then
    invalid_arg "Buf.remove: [at] must be >= 0";
  if len < 0 then
    invalid_arg "Buf.remove: [len] must be >= 0";
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
             T.join l (aux (-1) r))
        else
          (* At the right of the current node *)
          T.node l cell (aux (n - n1) r)
      end
      else
        T.node l cell r
  in
  if is_leaf t.tree then t
  else update t (aux at)

let remove_between t c1 c2 =
  validate t c1 "Buf.remove_between: cursor not in buffer";
  validate t c2 "Buf.remove_between: cursor not in buffer";
  if c1 == c2 then t
  else if compare c1 c2 > 0 then
    invalid_arg
      "Buf.remove_between: cursors must be in increaing order"
  else begin
    let rec cut_left = function
      | T.Leaf -> invalid_arg "Buf.remove_between: cursor not in buffer"
      | T.Node (_, l, (c, _ as cell), r, _) ->
        let c1 = compare c1 c in
        if c1 < 0 then
          cut_left l
        else if c1 > 0 then
          T.node l cell (cut_left r)
        else (* c1 = 0 *)
          T.node l cell T.leaf
    in
    let rec cut_right = function
      | T.Leaf -> invalid_arg "Buf.remove_between: cursor not in buffer"
      | T.Node (_, l, (c, _ as cell), r, _) ->
        let c2 = compare c2 c in
        if c2 > 0 then
          cut_right r
        else if c2 < 0 then
          T.node (cut_right l) cell r
        else (* c2 = 0 *)
          T.node T.leaf cell r
    in
    let rec aux = function
      | T.Leaf -> invalid_arg "Buf.remove_between: cursor not in buffer"
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
          T.join (cut_left l) (cut_right r)
    in
    update t aux
  end

let remove_after t c len =
  validate t c "Buf.remove_after: cursor not in buffer";
  if len < 0 then
    invalid_arg "Buf.remove_after: len must be >= 0"
  else if len = 0 then
    t
  else begin
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
          if !len <= n1 then begin
            let result = T.node T.leaf (c, n1 - !len) r in
            len := 0;
            result
          end else begin
            len := !len - n1;
            rem r
          end
        end
    in
    let rec seek = function
      | T.Leaf ->
        invalid_arg "Buf.remove_after: cursor not in buffer"
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
             T.join l (rem r))
        else (* c > 0 *)
          T.node l cell (seek r)
    in
    update t seek
  end

let remove_before t c len =
  validate t c "Buf.remove_before: cursor not in buffer";
  if len < 0 then
    invalid_arg "Buf.remove_before: len must be >= 0"
  else if len = 0 then
    t
  else begin
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
          let len' = !len in
          if len' <= n1 then begin
            len := - (n1 - !len);
            l
          end else begin
            len := !len - n1;
            rem l
          end
        end
    in
    let rec seek = function
      | T.Leaf ->
        invalid_arg "Buf.remove_before: cursor not in buffer"
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
             T.join l (shift (n - len') r))
          else
            (len := len' - n;
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
    update t seek
  end

let insert_before t c len =
  validate t c "Buf.insert_before: cursor not in buffer";
  if len < 0 then
    invalid_arg "Buf.insert_before: len must be >= 0"
  else if len = 0 then
    t
  else begin
    let rec aux = function
      | T.Leaf -> invalid_arg "Buf.insert_before: cursor not in buffer"
      | T.Node (_, l, (c', n as cell), r, _) ->
        let c = compare c c' in
        if c < 0 then
          T.node (aux l) cell r
        else if c > 0 then
          T.node l cell (aux r)
        else
          T.node l (c', n + len) r
    in
    update t aux
  end

let insert_after t c len =
  validate t c "Buf.insert_after: cursor not in buffer";
  if len < 0 then
    invalid_arg "Buf.insert_after: len must be >= 0"
  else if len = 0 then
    t
  else begin
    let len = ref len in
    let rec aux = function
      | T.Leaf -> invalid_arg "Buf.insert_after: cursor not in buffer"
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
    update t aux
  end


let put_before t c0 content =
  validate t c0 "Buf.put_before: cursor not in buffer";
  let c = {position = O.before c0.position; content} in
  let rec traverse = function
    | T.Leaf -> invalid_arg "Buf.put_before: cursor not in buffer"
    | T.Node (_, l, (c', offset as cell), r, _) ->
      let i = compare c0 c' in
      if i = 0 then
        T.node (T.node l (c, offset) T.leaf) (c', 0) r
      else if i < 0 then
        T.node (traverse l) cell r
      else
        T.node l cell (traverse r)
  in
  update t traverse, c

let put_after t c0 content =
  validate t c0 "Buf.put_after: cursor not in buffer";
  let c = {position = O.after c0.position; content} in
  let rec traverse = function
    | T.Leaf -> invalid_arg "Buf.put_after: cursor not in buffer"
    | T.Node (_, l, (c', _ as cell), r, _) ->
      let i = compare c0 c' in
      if i = 0 then
        T.node (T.node l cell T.leaf) (c, 0) r
      else if i < 0 then
        T.node (traverse l) cell r
      else
        T.node l cell (traverse r)
  in
  update t traverse, c

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

let cursor_before t c =
  validate t c "Buf.cursor_before: cursor not in buffer";
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
  aux t.tree

let cursor_after t c =
  validate t c "Buf.cursor_after: cursor not in buffer";
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
  aux t.tree
