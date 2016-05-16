open Sturgeon
open Session
open Inuit

let () =
  let fd = Unix.openfile "copycat.log"
      [Unix.O_CREAT; Unix.O_TRUNC; Unix.O_WRONLY] 0o660
  in
  Unix.dup2 fd Unix.stderr;
  Unix.close fd

let () =
  Recipes.text_command @@ fun ~args:_ shell ->
  let k = Stui.create_cursor shell ~name:"print-server" in
  text k "Hi, how are you doing?\n";
  let counter = ref 0 in
  link k ">>>>> 0 <<<<<"
    (fun k' ->
       clear k';
       incr counter;
       text k' (string_of_int !counter));
  text k "\n";
  let _ = Inuit_widget.Check.make k in
  text k " Check me\n";
  text k "Edit me: ";
  let k' = ref (Inuit.cursor_of_region Inuit.Region.null) in
  let _ = Inuit_widget.Edit.make k ~on_change:(fun t ->
      let str = Inuit_widget.Edit.state t in
      clear !k';
      text !k' str;
    ) in
  text k "\n";
  k' := sub k;
  text k "\n";
  text k "Adjust me: ";
  let _ = Inuit_widget.Slider.make k in
  ()

