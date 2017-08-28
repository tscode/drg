
open Ext_std
open Value


exception Form_error of string

let err name msg = Form_error ("Var '" ^ name ^ "' " ^ msg)

type t = (bytes * Raw.t) list [@@deriving show, yojson]

let create () = []

let read_raw_opt name form =
  try Some (List.assoc name form) with
  | Not_found -> None

let read_raw name form = 
  try List.assoc name form with
  | Not_found -> raise (err name "missing")

let read_raw_typed typ name form = 
  let raw = read_raw name form in
  if typ = Raw.to_type raw then raw
  else raise (err name "has wrong type")

let read_raw_listed l name form = 
  let raw = read_raw name form in
  if List.mem raw l then raw
  else raise (err name "is not listed correctly")

let write_raw name raw form = (name, raw) :: form
let write_named named form = Named.(name named, to_raw named) :: form

let finalize form =
  List.sort_uniq Util.cmp_assoc form

(*let unimplemented temp form =*)
  (*let temp' = template form in*)
  (*List.diff Pervasives.compare temp temp'*)

(*let implements temp form = *)
  (*unimplemented temp form = []*)

