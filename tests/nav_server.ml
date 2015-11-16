open Sturgeon
open Session
open Ui_print

let () =
  let fd = Unix.openfile "sturgeon.log"
      [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY] 0o660
  in
  Unix.dup2 fd Unix.stderr;
  Unix.close fd

let () =
  Recipes.text_command @@ fun ~args ~set_title k ->
  set_title "nav-server";
  Ui_nav.make k "Épiménide" @@ fun nav ~title ~body ->
  text body "Je mens.\n\n";
  link body "- C'est vrai.\n"
    (fun _ -> Ui_nav.modal nav "C'est vrai !" @@
      fun nav ~title ~body -> text body "C'est faux.");
  link body "- C'est faux.\n"
    (fun _ -> Ui_nav.modal nav "C'est faux !" @@
      fun nav ~title ~body -> text body "C'est vrai.")
