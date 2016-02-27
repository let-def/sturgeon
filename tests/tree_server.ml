open Sturgeon
open Session
open Inuit

let rec children prefix t =
  for i = 0 to 9 do
    let label = prefix ^ string_of_int i in
    text
      (Tree.add t ~children:(children label))
      label
  done

let () =
  Recipes.text_command
  @@ fun ~args ~set_title cursor ->
  set_title "tree-server";

  Nav.make cursor "0 to 9" @@ fun nav ->
  let body = Nav.body nav in
  text body "\n";
  children "/" (Tree.make body)
