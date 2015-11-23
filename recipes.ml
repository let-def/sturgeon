let command ?greetings ?cogreetings () =
  let stdin = Sexp.of_channel stdin in
  let stdout sexp =
    Sexp.tell_sexp print_string sexp;
    print_newline ();
    flush stdout
  in
  let stdin', status = Session.connect ?greetings ?cogreetings stdout in
  let rec aux () =
    match stdin () with
    | None -> exit 0
    | Some sexp ->
      stdin' sexp;
      if Session.pending_sessions status > 0 then
        aux ()
      else exit 0
  in
  aux ()

let text_command f =
  let open Sexp in
  command ~cogreetings:(function
      | C (S "textbuf", C (session, args)) ->
        let cursor, set_title = Stui.accept_cursor session in
        f ~args ~set_title cursor
      | sexp -> Session.cancel sexp
    )
    ()

type server = {
  greetings: (unit -> Session.t) option;
  cogreetings: (Session.t -> unit) option;

  mutable socket: Unix.file_descr option;
  mutable clients: (Unix.file_descr) list;
  connections:
    (Unix.file_descr,
     (unit -> Sexp.basic option) * Session.output * Session.status)
    Hashtbl.t;
}

let server ?greetings ?cogreetings name =
  let dir = Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "sturgeon.%d" (Unix.getuid ())) in
  if not (Sys.file_exists dir) then
    Unix.mkdir dir 0o770;
  let name = Filename.concat dir
      (Printf.sprintf "%s.%d.sturgeon" name (Unix.getpid ())) in
  let socket = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let addr = Unix.ADDR_UNIX name in
  Unix.bind socket addr;
  at_exit (fun () -> Unix.unlink name);
  Unix.listen socket 3;
  { socket = Some socket; clients = []; connections = Hashtbl.create 7;
    greetings; cogreetings }

let accept server =
  match server.socket with
  | None -> ()
  | Some socket ->
    let client, _ = Unix.accept socket in
    let oc = Unix.out_channel_of_descr client in
    let send sexp =
      Sexp.tell_sexp (output_string oc) sexp;
      output_char oc '\n';
      flush oc
    in
    let cogreetings = server.cogreetings in
    let greetings = match server.greetings with
      | None -> None
      | Some f -> Some (f ())
    in
    let received, status = Session.connect ?greetings ?cogreetings send in
    let stdin = Sexp.of_file_descr ~on_read:ignore client in
    server.clients <- client :: server.clients;
    Hashtbl.replace server.connections
      client (stdin, received, status)

let filter_fd server fd =
  if Hashtbl.mem server.connections fd then true
  else begin
    Unix.close fd;
    false
  end

let rec main_loop server =
  match server.socket with
  | None -> ()
  | Some socket ->
    server.clients <- List.filter (filter_fd server) server.clients;
    let r, _, _ = Unix.select (socket :: server.clients) [] [] 1.0 in
    let rec pump fd (stdin, received, status) =
      match stdin () with
      | None -> Hashtbl.remove server.connections fd
      | Some sexp ->
        received sexp;
        if Unix.select [fd] [] [] 0.0 <> ([],[],[]) then
          pump fd (stdin, received, status)
    in
    let process fd =
      if fd = socket then
        accept server
      else
        pump fd (Hashtbl.find server.connections fd)
    in
    List.iter process r;
    main_loop server

let stop_server server =
  match server.socket with
  | None -> ()
  | Some socket ->
    server.socket <- None;
    server.clients <- [];
    let clients = server.clients in
    Hashtbl.clear server.connections;
    List.iter Unix.close clients;
    Unix.close socket

let text_server name f =
  let open Sexp in
  server ~cogreetings:(function
      | C (S "textbuf", C (session, args)) ->
        let cursor, set_title = Stui.accept_cursor session in
        f ~args ~set_title cursor
      | sexp -> Session.cancel sexp
    ) name
