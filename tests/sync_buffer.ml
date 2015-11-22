open Sturgeon
open Sexp
open Session

let editors = ref []

let buffer_accept = function
  | M (Sink t) ->
    let handler, buffer = Ui_buf.create () in
    t (Feed (C (S "accept", handler)));
    buffer, (fun str -> t (Feed (C (S "title", T str))))
  | _ -> invalid_arg "Ui_print.accept"

let on_change editor ~start ~old_len ~text_raw ~text_len ~text ~clickable =
  prerr_endline text;
  List.iter (fun editor' ->
      if editor' != editor then
        Ui_buf.change editor' ~start ~old_len ~text_raw ~text ~clickable)
    !editors

let () =
  let open Sexp in
  Recipes.main_loop @@
  Recipes.server ~cogreetings:(function
      | C (S "ui-text", C (session, args)) ->
        let buffer, set_title = buffer_accept session in
        set_title "test";
        editors := Ui_buf.edit buffer ~on_change ~on_click:(fun _ _ -> ()) :: !editors
      | sexp -> Session.cancel sexp
    ) "sync"
