
open Ext_std
open Value


exception Form_error of string

let form_error_str name msg = "Variable '" ^ name ^ "' " ^ msg
let form_error name msg = error (form_error_str name msg)


type t = (bytes * Raw.t) list [@@deriving show, yojson]

let create () = []

let read_raw name form =
  try Ok (List.assoc name form) with
  | Not_found -> form_error name "missing"

let read_raw_exn name form = 
  try List.assoc name form with
  | Not_found -> raise (Form_error (form_error_str name "missing"))

let read_raw_typed typ name form = 
  match read_raw name form with
  | Error msg -> error msg
  | Ok raw ->
    match typ = Raw.to_type raw with
    | true  -> ok raw
    | false -> form_error name "has wrong type"
  
let read_raw_listed l name form = 
  match read_raw name form with
  | Error msg -> error msg
  | Ok raw ->
    match List.mem raw l with
    | true  -> ok raw
    | false -> form_error name "is not listed correctly"

let write_raw name raw form = (name, raw) :: form
let write_named named form = Named.(p_name named, p_raw named) :: form
let finalize form = List.sort_uniq Util.cmp_assoc form


