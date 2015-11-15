open Sturgeon
open Session
open Ui_print

let () =
  let fd = Unix.openfile "copycat.log"
      [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY] 0o660
  in
  Unix.dup2 fd Unix.stderr;
  Unix.close fd

let () =
  Recipes.text_command @@ fun ~args ~set_title k ->
  set_title "print-server";
  text k "Hi, how are you doing?\n";
  let counter = ref 0 in
  link k ">>>>> 0 <<<<<"
    (fun k' ->
       clear k';
       incr counter;
       text k' (string_of_int !counter))
