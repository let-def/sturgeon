open Sturgeon
open Sexp
open Session

let new_cp c = Char.code c land 0xC0 <> 0x80

let find_pos s pos count =
  let pos = ref pos in
  let count = ref count in
  while !count > 0 do
    if new_cp s.[!pos] then
      decr count;
    incr pos
  done;
  !pos

let apply_change s txt =
  let open Tui.Textbuf in
  let pos1 = find_pos s txt.position 0 in
  let pos2 = find_pos s txt.old_len pos1 in
  String.sub s 0 pos1 ^ txt.text ^ String.sub s pos2 (String.length s - pos2)

let buffer = ref ""

let apply_change txt =
  buffer := apply_change !buffer txt

module Hub : sig
  type t
  val make : unit -> t
  val port : t -> Tui.textbuf
end = struct
  type t = item list ref
  and item = {
    mutable buf: Tui.textbuf;
    hub: t;
  }

  let class_ = {
    Tui.Class.
    connect = (fun r t -> r.buf <- t);
    connected = ignore;
    change = (fun r txt ->
        List.iter (fun r' ->
            let open Tui.Textbuf in
            apply_change txt;
            let txt = text
                ~raw:txt.text_raw
                ~editable:true
                txt.position
                txt.old_len
                txt.text
            in
            if r != r' then Tui.Textbuf.change r'.buf txt)
          !(r.hub));
    click = (fun r off ->
        List.iter (fun r' ->
            if r != r' then Tui.Textbuf.click r'.buf off)
          !(r.hub));
  }

  let make () = ref []

  let port hub =
    let item = { buf = Tui.Textbuf.null; hub } in
    hub := item :: !hub;
    Tui.Class.make_textbuf class_ item
end

let () =
  ignore (Sys.signal Sys.sigpipe Sys.Signal_ignore);
  let open Sexp in
  Recipes.main_loop @@
  let hub = Hub.make () in
  Recipes.server ~cogreetings:(function
      | C (S "textbuf", C (session, args)) ->
        let a, set_title = Stui.accept_textbuf session in
        set_title "test";
        Tui.Textbuf.change a (Tui.Textbuf.text ~editable:true 0 0 !buffer);
        let b = Hub.port hub in
        Tui.Textbuf.connect ~a ~b
      | sexp -> Session.cancel sexp
    ) "sync"
