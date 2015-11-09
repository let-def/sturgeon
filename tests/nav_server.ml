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


let () =
  let open Emacs_hyperprint in
  Emacs_hypernav.navigator (open_buffer endpoint "nav-server")
    "Epimenide"
  @@ fun nav ~title ~body ->
  text body "Je mens.\n\n";
  link body "- C'est vrai.\n"
    (fun _ -> Emacs_hypernav.goto nav "C'est vrai !" @@
      fun nav ~title ~body -> text body "C'est faux.");
  link body "- C'est faux.\n"
    (fun _ -> Emacs_hypernav.goto nav "C'est faux !" @@
      fun nav ~title ~body -> text body "C'est vrai.")

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
