open Sexp
open Inuit

type 'flags text = {
  offset  : int;
  old_len : int;
  new_len : int;
  text    : string;
  flags   : 'flags list;
} constraint 'flags = [> flags]

let text_length ?(flags=[]) str =
  if List.mem `Raw flags then String.length str
  else
    let count = ref 0 in
    for i = 0 to String.length str - 1 do
      let c = Char.code str.[i] in
      if c land 0xC0 <> 0x80 then
        incr count
    done;
    !count

let text ?(flags=[]) start len text =
  {
    offset = start;
    old_len = len;
    new_len = text_length ~flags text;
    text    = text;
    flags   = flags;
  }

class type ['flags] t =
  object
    method connect: 'flags t -> unit
    method change: 'flags text -> unit
    method click: int -> unit
    constraint 'flags = [> flags]
  end

type simple = flags t

let change t text = t#change text

let click t pos = t#click pos

let connect ~a ~b =
  a#connect (b : 'flags #t :> 'flags t);
  b#connect (a : 'flags #t :> 'flags t)

let null = object
  method connect = ignore
  method change = ignore
  method click = ignore
end

let get_action c = (Lazy.force (Trope.content c))#action

class virtual ['flags] bufcursor =
  object (self : 'self)
    val virtual trope : 'self lazy_t Trope.t ref
    val virtual beginning : 'self lazy_t Trope.cursor
    val virtual position  : 'self lazy_t Trope.cursor

    val mutable textbuf : 'flags t = null

    val action : ('self -> unit) option = None
    method action =
      match action with
      | None -> ()
      | Some action -> action self

    (* cursor *)

    method is_closed = not (Trope.member !trope position)

    method clear =
      if Trope.member !trope position then begin
        let start = Trope.position !trope beginning in
        let length = Trope.position !trope position - start in
        change textbuf (text start length "");
        trope := Trope.remove_between !trope beginning position
      end

    method text ?(flags=[]) str =
      if Trope.member !trope position then begin
        let start = Trope.position !trope position in
        let flags = match action with
          | None -> flags
          | Some action -> `Clickable :: flags
        in
        let text = text ~flags start 0 str in
        change textbuf text;
        trope := Trope.insert_before !trope position text.new_len
      end

    method sub_action action : 'self =
      if self#is_closed then (self : 'self) else
        let rec cursor = lazy begin
          let t, beginning = Trope.put_before !trope position cursor in
          let t, position  = Trope.put_after t beginning cursor in
          trope := t;
          ({< beginning = beginning; position = position; action = action >} : 'self)
        end in
        Lazy.force cursor

    method sub = self#sub_action action

    (* textbuf *)
    constraint 'flags = [> flags]

    method connect buf =
      textbuf <- buf

    method change (text : 'flags text) =
      if text.old_len <> 0 then
        trope := Trope.remove ~at:text.offset ~len:text.old_len !trope;
      if text.new_len <> 0 then
        trope := Trope.insert ~at:text.offset ~len:text.new_len !trope

    method click offset =
      match Trope.find_before !trope offset with
      | None -> ()
      | Some cursor -> get_action cursor
  end

let with_cursor () =
  let rec cursor = lazy begin
    let trope0, beginning0 = Trope.put_cursor (Trope.create ()) ~at:0 cursor in
    let trope0, position0  = Trope.put_after  trope0 beginning0 cursor in
    object
      inherit [[> flags]] bufcursor
      val trope = (ref trope0)
      val beginning = beginning0
      val position = position0
    end
  end in
  let lazy cursor = cursor in
  (cursor : _ bufcursor :> _ Inuit.cursor),
  (cursor : _ bufcursor :> _ t)
