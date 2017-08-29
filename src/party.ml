
open Ext_std
open Value

exception Party_error

type t = {
  members : Member.t Id.w_id list;
  rows    : Row.t    Id.w_id list
} [@@deriving show, yojson]

let members p = p.members
let names   p = members p |> List.map Id.dat
let rows    p = p.rows    |> List.map Id.dat

let create () = { members = []; rows = [] }

let has_member mname party =
  List.exists (Member.has_name mname) (names party)

let has_member_w_id id party =
  List.exists (Id.has_id id) (members party)

let obscure ?except_id party =
  let members = Id.map_w_id (fun id x ->
    if Some id = except_id then x else Member.obscure x
  ) party.members in
  { party with members }

let has_row rname party =
  List.exists (Row.has_name rname) (rows party)

let has_row_w_id id party =
  List.exists (Id.has_id id) party.rows

let is_authorized_w_id id pwd party =
  try Member.pwd (Id.find_w_id id (members party)) = pwd
  with Not_found -> false

let member_w_id id party =
  Id.find_w_id id (members party)

let name_w_id id party =
  Id.find_w_id id (members party) |> Member.name

let id_w_name name party =
  Id.find_id (Member.has_name name) (members party)

let add_member ?(date=Date.now ()) ~id mname pwd party =
  if has_member mname party then raise Party_error
  else {
    members = Id.w_id id (Member.create mname pwd) :: (members party);
    rows = Id.map (Row.add_cell ~date) party.rows
  }

let del_member_idx n party = 
  if n = -1 then raise Party_error
  else {
    members = Id.remi (fun j _ -> (j = n)) (members party);
    rows = Id.map (Row.del_cell n) party.rows
  }
    
let del_member mname party =
  let n = List.find_idx (Member.has_name mname) (names party) in
  del_member_idx n party

let del_member_w_id id party =
  let n = List.find_idx (Id.has_id id) (members party) in
  del_member_idx n party

let add_row f ?(date=Date.now ()) ~id name def party = 
  if has_row name party then raise Party_error
  else
  let row = f ~date (List.length (members party)) name def in
  { party with rows = Id.w_id id row :: party.rows }

let add_int_row    = add_row Row.int
let add_float_row  = add_row Row.float 
let add_bool_row   = add_row Row.bool
let add_string_row = add_row Row.string


let del_row rname party =
  let rows = Id.rem (Row.has_name rname) party.rows in
  { party with rows }

let del_row_w_id id party =
  let rows = List.rem (Id.has_id id) party.rows in
  { party with rows }


let get_row rname party = 
  List.find (Row.has_name rname) (rows party)

let get_row_w_id id party = 
  List.find (Id.has_id id) party.rows 


let push f ?(date=Date.now ()) mname rname v party =
  let n = List.find_idx (Member.has_name mname) (names party) in
  let rows = Id.map
    (fun r -> if Row.has_name rname r then f ~date n v r else r)
    party.rows
  in 
  { party with rows }

let push_int    = push Row.push_int
let push_float  = push Row.push_float
let push_bool   = push Row.push_bool
let push_string = push Row.push_string


let get f mname rname party =
  let n = List.find_idx (Member.has_name mname) (names party) in
  let r = List.find (Row.has_name rname) (rows party) in
  f n r

let get_w_id f mid rid party =
  let n = List.find_idx (Id.has_id mid) (members party) in
  let r = List.find (Id.has_id rid) party.rows |> Id.dat in
  f n r


(*let get_multi get mname rname party =*)
  (*get_multi_w_date get mname rname party |> List.map Date.strip*)

(*let get_single_w_date get mname rname party =*)
  (*get_multi_w_date get mname rname party |> List.hd*)

(*let get_single get mname rname party =*)
  (*get_multi_w_date get mname rname party |> List.hd |> Date.strip *)

(*let get_int  = get_single Named.get_int_cell*)
(*let get_ints = get_multi Named.get_int_cell*)
(*let get_int_w_date  = get_single_w_date Named.get_int_cell*)
(*let get_ints_w_date = get_multi_w_date Named.get_int_cell*)

(*let get_float  = get_single Named.get_float_cell*)
(*let get_floats = get_multi Named.get_float_cell*)
(*let get_float_w_date  = get_single_w_date Named.get_float_cell*)
(*let get_floats_w_date = get_multi_w_date Named.get_float_cell*)

(*let get_bool  = get_single Named.get_bool_cell*)
(*let get_bools = get_multi Named.get_bool_cell*)
(*let get_bool_w_date  = get_single_w_date Named.get_bool_cell*)
(*let get_bools_w_date = get_multi_w_date Named.get_bool_cell*)

(*let get_string  = get_single Named.get_string_cell*)
(*let get_strings = get_multi Named.get_string_cell*)
(*let get_string_w_date  = get_single_w_date Named.get_string_cell*)
(*let get_strings_w_date = get_multi_w_date Named.get_string_cell*)

(*let get_default_value mname rname party =*)
  (*let i = List.find_idx (Member.has_name mname) party.members in*)
  (*let row = List.find (fun row -> Named.row_name row = rname) party.rows in*)
  (*Named.row_value row*)


(*let get_default_single mname rname party = *)
  (*get_default_value mname rname party |> Named.to_single*)
