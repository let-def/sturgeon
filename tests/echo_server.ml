open Sturgeon
open Session

let () =
  let fd = Unix.openfile "copycat.log"
      [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY] 0o660
  in
  Unix.dup2 fd Unix.stderr;
  Unix.close fd

let endpoint = connect (fun ~remote_query ->
    {
      stdout = (fun sexp ->
          prerr_string "> ";
          Sexp.tell_sexp prerr_string sexp;
          prerr_newline ();
          Sexp.tell_sexp print_string sexp;
          print_newline ();
        );
      query = remote_query;
    }
  )

let reader = Sexp.of_channel stdin

let rec loop () =
  flush_all ();
  match reader () with
  | None -> close endpoint
  | Some sexp ->
    prerr_string "< ";
    Sexp.tell_sexp prerr_string sexp;
    prerr_newline ();
    endpoint.stdout sexp;
    loop ()

let () = loop ()
