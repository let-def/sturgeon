open Emacs_serge
open Emacs_sexp

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
    query = (function
        | C (S "fib", C (I n, C (M (Sink f), S "nil"))) ->
          let j0 = ref 1 in
          let j1 = ref 1 in
          for i = 0 to n - 1 do
            f (Feed (I !j1));
            let j0' = !j0 in
            j0 := !j1;
            j1 := j0' + !j1
          done;
          f (Quit sym_nil)
        | q -> cancel q);
  }

let reader = Emacs_sexp.of_channel stdin

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
