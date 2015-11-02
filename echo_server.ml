open Emacs_serge

let () =
  let fd = Unix.openfile "copycat.log"
      [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY] 0o660
  in
  Unix.dup2 fd Unix.stderr;
  Unix.close fd

let endpoint = ref {
    stdout = (fun _ -> assert false);
    query = (fun _ -> assert false);
}

let () = endpoint := connect {
    stdout = (fun sexp ->
        prerr_string "> ";
        Emacs_sexp.tell_sexp prerr_string sexp;
        prerr_newline ();
        Emacs_sexp.tell_sexp print_string sexp;
        print_newline ();
      );
    query = (fun t -> (!endpoint).query t);
  }

let reader = Emacs_sexp.of_channel stdin

let rec loop () =
  flush_all ();
  match reader () with
  | None -> close !endpoint
  | Some sexp ->
    prerr_string "< ";
    Emacs_sexp.tell_sexp prerr_string sexp;
    prerr_newline ();
    (!endpoint).stdout sexp;
    loop ()

let () = loop ()
