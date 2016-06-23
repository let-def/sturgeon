open Sturgeon
open Session
open Inuit

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
  let change s patch =
    let open Inuit.Patch in
    let pos1 = find_pos s patch.offset 0 in
    let pos2 = find_pos s (Patch.removed patch) pos1 in
    String.sub s 0 pos1 ^ Patch.inserted_text patch ^ String.sub s pos2 (String.length s - pos2)
  in
  fun txt -> buffer := change !buffer txt

let clients = ref []

let lock = Mutex.create ()
let client_change client patch =
  Mutex.lock lock;
  prerr_endline "change";
  match
    apply_change patch;
    List.iter (fun client' ->
        if client != client' then
          Inuit.Socket.send client' patch
      ) !clients
  with
  | () -> Mutex.unlock lock
  | exception exn -> Mutex.unlock lock; raise exn

let () =
  ignore (Sys.signal Sys.sigpipe Sys.Signal_ignore);
  let open Sexp in
  let socket_connect t () =
    prerr_endline "Client connected";
    Socket.send t (Patch.make ~offset:0 [`Editable] (Patch.Insert !buffer));
    Socket.set_receive t (client_change t);
    clients := t :: !clients
  in
  let socket_close t () =
    prerr_endline "Client disconnected";
    clients := List.filter ((!=) t) !clients
  in
  let server = Sturgeon_recipes_server.text_server "sync-text" (fun ~args shell ->
      prerr_endline "New client";
      let socket = Inuit.Socket.make ~receive:ignore in
      Socket.set_on_connected socket (socket_connect socket);
      Socket.set_on_closed socket (socket_close socket);
      Stui.create_buffer shell ~name:"test" (Socket.endpoint socket)
    )
  in
  Sturgeon_recipes_server.main_loop server
