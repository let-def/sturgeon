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

open Lwt

type server = {
  greetings: (unit -> Session.t) option;
  cogreetings: (Session.t -> unit) option;

  mutable socket: Lwt_unix.file_descr option;
}

let server ?greetings ?cogreetings name =
  let dir = Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "sturgeon.%d" (Unix.getuid ())) in
  if not (Sys.file_exists dir) then
    Unix.mkdir dir 0o770;
  let name = Filename.concat dir
      (Printf.sprintf "%s.%d.sturgeon" name (Unix.getpid ())) in
  let socket = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let addr = Lwt_unix.ADDR_UNIX name in
  Lwt_unix.bind socket addr;
  at_exit (fun () -> Unix.unlink name);
  Lwt_unix.listen socket 3;
  { socket = Some socket; greetings; cogreetings }

let accept server =
  match server.socket with
  | None -> return_unit
  | Some socket ->
    Lwt_unix.accept socket >|= fun (client, _) ->
    let fd = Lwt_unix.unix_file_descr client in
    let oc = Unix.out_channel_of_descr fd in
    let ic = Lwt_io.of_fd ~mode:Lwt_io.input client in
    let send sexp =
      try
        Sexp.tell_sexp (output_string oc) sexp;
        output_char oc '\n';
        flush oc
      with _ -> () (* Can raise broken pipe *)
    in
    let cogreetings = server.cogreetings in
    let greetings = match server.greetings with
      | None -> None
      | Some f -> Some (f ())
    in
    let received, status = Session.connect ?greetings ?cogreetings send in
    let rec loop () =
      Lwt_io.read_line_opt ic >>= function
      | None -> Lwt_unix.close client
      | Some str ->
        received (Sexp.of_string str);
        loop ()
    in
    Lwt.async loop

let text_server name f =
  let open Sexp in
  server ~cogreetings:(function
      | C (S "textbuf", C (session, args)) ->
        let cursor, set_title = Stui.accept_cursor session in
        f ~args ~set_title cursor
      | sexp -> Session.cancel sexp
    ) name
