open Sturgeon
open Session
open Sexp

let endpoint = connect @@ fun ~remote_query:_ ->
  {
    stdout = (fun sexp ->
        prerr_string "> ";
        Sexp.tell_sexp prerr_string sexp;
        prerr_newline ();
        Sexp.tell_sexp print_string sexp;
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
