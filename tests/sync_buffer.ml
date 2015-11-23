open Sturgeon
open Sexp
open Session

let buffer_accept = function
  | M (Sink t) ->
    let handler, buffer = Stui.textbuf_session () in
    t (Feed (C (S "accept", handler)));
    buffer, (fun str -> t (Feed (C (S "title", T str))))
  | _ -> invalid_arg "Ui_print.accept"

module Hub : sig
  type t
  val make : unit -> t
  val port : t -> Tui.textbuf
end = struct
  type t = item list ref
  and item = {
    mutable buf: Tui.textbuf;
    hub: t;
  }

  let class_ = {
    Tui.Class.
    connect = (fun r t -> r.buf <- t);
    connected = ignore;
    change = (fun r text ->
        List.iter (fun r' ->
            if r != r' then Tui.Textbuf.direct_change r'.buf text)
          !(r.hub));
    click = (fun r off ->
        List.iter (fun r' ->
            if r != r' then Tui.Textbuf.click r'.buf off)
          !(r.hub));
  }

  let make () = ref []

  let port hub =
    let item = { buf = Tui.Textbuf.null; hub } in
    hub := item :: !hub;
    Tui.Class.make_textbuf class_ item
end

let () =
  let open Sexp in
  Recipes.main_loop @@
  let hub = Hub.make () in
  Recipes.server ~cogreetings:(function
      | C (S "ui-text", C (session, args)) ->
        let a, set_title = buffer_accept session in
        set_title "test";
        let b = Hub.port hub in
        Tui.Textbuf.connect ~a ~b
      | sexp -> Session.cancel sexp
    ) "sync"
