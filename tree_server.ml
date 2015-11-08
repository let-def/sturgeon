open Emacs_serge

let () =
  let fd = Unix.openfile "copycat.log"
      [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY] 0o660
  in
  Unix.dup2 fd Unix.stderr;
  Unix.close fd

let endpoint = connect @@ fun ~remote_query:_ ->
  {
    stdout = (fun sexp ->
        prerr_string "> ";
        Emacs_sexp.tell_sexp prerr_string sexp;
        prerr_newline ();
        Emacs_sexp.tell_sexp print_string sexp;
        print_newline ();
      );
    query = (fun t -> cancel t);
  }

let reader = Emacs_sexp.of_channel stdin

let rec children prefix t =
  for i = 0 to 999 do
    let label = prefix ^ string_of_int i in
    Emacs_hyperprint.text
      (Emacs_htree.add t ~children:(children label))
      label
  done

let () =
  let open Emacs_hyperprint in
  Emacs_hypernav.navigator (open_buffer endpoint "tree-server")
    "Epimenide"
  @@ fun nav ~title ~body ->
  text body "\n";
  children "/" (Emacs_htree.make body)

let rec loop () =
  flush_all ();
  match reader () with
  | None -> close endpoint
  | Some sexp ->
    prerr_string "< ";
    Emacs_sexp.tell_sexp prerr_string sexp;
    prerr_newline ();
    endpoint.stdout sexp;
    loop ()

let () = loop ()
