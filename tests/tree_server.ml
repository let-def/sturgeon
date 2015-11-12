open Sturgeon
open Session

let () =
  let fd = Unix.openfile "copycat.log"
      [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY] 0o660
  in
  Unix.dup2 fd Unix.stderr;
  Unix.close fd

let rec children prefix t =
  for i = 0 to 999 do
    let label = prefix ^ string_of_int i in
    Ui_print.text
      (Ui_tree.add t ~children:(children label))
      label
  done

let connect_ui cursor =
  Ui_nav.navigator cursor "Epimenide"
  @@ fun nav ~title ~body ->
  Ui_print.text body "\n";
  children "/" (Ui_tree.make body)

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
        | Sexp.C (Sexp.S "connect-ui", Sexp.C (session, _)) ->
          let cursor, title = Ui_print.accept session in
          title "tree-server";
          connect_ui cursor
        | t ->
          Sexp.tell_sexp prerr_string (Sexp.transform_cons ~inj:(fun _ -> Sexp.sym_nil) ~map:(fun x -> x) t);
          prerr_newline ();
          cancel t);
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
