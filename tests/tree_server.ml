open Sturgeon
open Session

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
        Sexp.tell_sexp prerr_string sexp;
        prerr_newline ();
        Sexp.tell_sexp print_string sexp;
        print_newline ();
      );
    query = (fun t -> cancel t);
  }

let reader = Sexp.of_channel stdin

let rec children prefix t =
  for i = 0 to 999 do
    let label = prefix ^ string_of_int i in
    Ui_print.text
      (Ui_tree.add t ~children:(children label))
      label
  done

let () =
  let open Ui_print in
  Ui_nav.navigator (open_buffer endpoint "tree-server")
    "Epimenide"
  @@ fun nav ~title ~body ->
  text body "\n";
  children "/" (Ui_tree.make body)

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
