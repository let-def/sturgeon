open Sturgeon
open Sexp
open Session
open Textbuf

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

let buffer = ref ""

let apply_change =
  let change s txt =
    let pos1 = find_pos s txt.offset 0 in
    let pos2 = find_pos s txt.old_len pos1 in
    String.sub s 0 pos1 ^ txt.text ^ String.sub s pos2 (String.length s - pos2)
  in
  fun txt -> buffer := change !buffer txt

class client (hub : client list ref) =
  object (self)
    val mutable remote : Textbuf.simple = Textbuf.null
    method connect buf = remote <- buf

    method remote_change txt =
      remote#change txt

    method change (txt : Inuit.flags Textbuf.text) =
      apply_change txt;
      List.iter (fun (client : client) ->
          if client <> (self :> client) then
            let txt = text
                ~flags:(`Editable :: txt.flags)
                txt.offset
                txt.old_len
                txt.text
            in
            client#remote_change txt)
        !hub

    method remote_click ofs =
      remote#click ofs

    method click ofs =
      List.iter (fun (client : client) ->
          if client <> (self :> client) then
            client#remote_click ofs
        ) !hub

    initializer
      hub := (self :> client) :: !hub
  end

let () =
  ignore (Sys.signal Sys.sigpipe Sys.Signal_ignore);
  let open Sexp in
  let hub = ref [] in
  let lock = Mutex.create () in
  let server = Recipes.server ~cogreetings:(fun arg ->
      Mutex.lock lock;
      match
        begin match arg with
          | C (S "textbuf", C (session, args)) ->
            let a, set_title = Stui.accept_textbuf session in
            set_title "test";
            Textbuf.change a (Textbuf.text ~flags:[`Editable] 0 0 !buffer);
            let b = new client hub in
            Textbuf.connect ~a ~b
          | sexp -> Session.cancel sexp
        end
      with
      | x -> Mutex.unlock lock; x
      | exception exn -> Mutex.unlock lock; raise exn
    ) "sync"
  in
  Recipes.main_loop server
  (*let rec loop () =
      Lwt.bind (Recipes.accept server) loop
    in
    Lwt_main.run (loop ())*)
