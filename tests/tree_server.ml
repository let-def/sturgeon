open Sturgeon
open Session

let rec children prefix t =
  for i = 0 to 9 do
    let label = prefix ^ string_of_int i in
    Ui_print.text
      (Ui_tree.add t ~children:(children label))
      label
  done

let () =
  Recipes.text_command
  @@ fun ~args ~set_title cursor ->
  set_title "tree-server";

  Ui_nav.make cursor "0 to 9"
  @@ fun nav ~title ~body ->
  Ui_print.text body "\n";
  children "/" (Ui_tree.make body)
