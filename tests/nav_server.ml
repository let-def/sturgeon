open Sturgeon
open Session
open Tui

let () =
  let fd = Unix.openfile "sturgeon.log"
      [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY] 0o660
  in
  Unix.dup2 fd Unix.stderr;
  Unix.close fd

let () =
  Recipes.text_command @@ fun ~args ~set_title k ->
  set_title "nav-server";
  Nav.make k "Épiménide" @@ fun nav body ->
  text body "Je mens.\n\n";
  link body "- C'est vrai."
    (fun _ -> Nav.modal nav "C'est vrai !" @@
      fun nav body -> text body "C'est faux.");
  text body "\n";
  link body "- C'est faux."
    (fun _ -> Nav.modal nav "C'est faux !" @@
      fun nav body -> text body "C'est vrai.");
  text body "\n"
