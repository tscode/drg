(* TODO: deleting does not throw errors, as it should! *)

open Ext_std
open Id
open Value


type t = {
  members : Member.t Counted.clist;
  rows    : Row.t    Counted.clist
} [@@deriving show, yojson]

let create ?(members=[]) ?(rows=[]) () = { members; rows }

let p_members p = p.members
let p_rows    p = p.rows

let p_members_dat p = List.map Counted.p_dat (p_members p)
let p_rows_dat    p = List.map Counted.p_dat (p_rows p)

let p_members_name p = List.map Member.p_name (p_members_dat p)
let p_rows_name    p = List.map Row.p_name    (p_rows_dat p)


let num_members p = List.length (p_members p)
let num_rows    p = List.length (p_rows    p)

let has_member_w_name mname party =
  List.exists (Member.has_name mname) (p_members_dat party)

let has_member_w_id id party =
  List.exists (Counted.has_id id) (p_members party)

let has_member_w_ion ion party = match ion with
  | Ion.Id id -> has_member_w_id id party
  | Ion.Name name -> has_member_w_name name party

let has_row_w_name rname party =
  List.exists (Row.has_name rname) (p_rows_dat party)

let has_row_w_id id party =
  List.exists (Counted.has_id id) (p_rows party)

let has_row_w_ion ion party = match ion with
  | Ion.Id id -> has_row_w_id id party
  | Ion.Name name -> has_row_w_name name party

let member_w_name name party =
  List.find (Member.has_name name) (p_members_dat party)

let member_id_w_name name party =
  Counted.List.find (Member.has_name name) (p_members party) 
  |> Counted.p_id 

let member_w_id id party =
  List.find (Counted.has_id id) (p_members party) 
  |> Counted.p_dat

let member_w_ion ion party = match ion with
  | Ion.Id id -> member_w_id id party
  | Ion.Name name -> member_w_name name party

let counted_member_w_id id party =
  List.find (Counted.has_id id) (p_members party) 

let counted_member_w_name name party =
  List.find (Member.has_name name $ Counted.p_dat) (p_members party)

let counted_member_w_ion ion party = match ion with
  | Ion.Id id -> counted_member_w_id id party
  | Ion.Name name -> counted_member_w_name name party

let member_name_w_id id party =
  List.find (Counted.has_id id) (p_members party) 
  |> Counted.p_dat 
  |> Member.p_name 

let member_name_w_ion ion party = match ion with
  | Ion.Id id -> member_name_w_id id party
  | Ion.Name name -> match has_member_w_name name party with 
    | true -> name
    | false -> raise Not_found


let row_w_name name party =
  List.find (Row.has_name name) (p_rows_dat party)

let row_w_id id party =
  List.find (Counted.has_id id) (p_rows party) 
  |> Counted.p_dat

let row_w_ion ion party = match ion with
  | Ion.Id id -> row_w_id id party
  | Ion.Name name -> row_w_name name party


let counted_row_w_id id party =
  List.find (Counted.has_id id) (p_rows party) 

let counted_row_w_name name party =
  List.find (Row.has_name name $ Counted.p_dat) (p_rows party)

let counted_row_w_ion ion party = match ion with
  | Ion.Id id -> counted_row_w_id id party
  | Ion.Name name -> counted_row_w_name name party


let row_name_w_id id party =
  List.find (Counted.has_id id) (p_rows party) 
  |> Counted.p_dat 
  |> Row.p_name 

let row_id_w_name name party =
  Counted.List.find (Row.has_name name) (p_rows party) 
  |> Counted.p_id 



let add_member ?(date=Date.now ()) id mname pwd party =
  match has_member_w_name mname party with
  | true -> raise Exists
  | false -> {
    members = Counted.create id (Member.create mname pwd) :: (p_members party);
    rows = Counted.List.map (Row.add_cell ~date) (p_rows party)
  }

let del_member n party =  match n < 0 with
  | true -> raise (Invalid_argument "del_member")
  | false -> {
    members = Counted.List.remi (fun j _ -> (j = n)) (p_members party);
    rows = Counted.List.map (Row.del_cell n) (p_rows party)
  } 
    
let del_member_w_name mname party =
  let n = List.find_index (Member.has_name mname) (p_members_dat party) in 
  del_member n party

let del_member_w_id id party =
  let n = List.find_index (Counted.has_id id) (p_members party) in
  del_member n party

let del_member_w_ion ion party = match ion with
  | Ion.Id id -> del_member_w_id id party
  | Ion.Name name -> del_member_w_name name party

let add_row f ?(date=Date.now ()) id name def party = 
  match has_row_w_name name party with
  | true -> raise Exists
  | false -> 
    let row = f ~date (List.length (p_members party)) name def in
    { party with rows = Counted.create id row :: p_rows party }

let add_int_row    = add_row Row.int
let add_float_row  = add_row Row.float 
let add_bool_row   = add_row Row.bool
let add_string_row = add_row Row.string


let del_row_w_name rname party =
  let rows = Counted.List.rem (Row.has_name rname) (p_rows party) in
  { party with rows }

let del_row_w_id id party =
  let rows = List.rem (Counted.has_id id) (p_rows party) in
  { party with rows }

let del_row_w_ion ion party = match ion with
  | Ion.Id id -> del_row_w_id id party
  | Ion.Name name -> del_row_w_name name party

let get_row_w_name rname party = 
  List.find (Row.has_name rname) (p_rows_dat party)

let get_row_w_id id party = 
  List.find (Counted.has_id id) (p_rows party)


let push f ?(date=Date.now ()) mname rname v party =
  let n = List.find_index (Member.has_name mname) (p_members_dat party) in
  let rows = Counted.List.map
    (fun r -> if Row.has_name rname r then f ~date n v r else r)
    party.rows
  in 
  { party with rows }

let push_raw    = push Row.push_raw
let push_int    = push Row.push_int
let push_float  = push Row.push_float
let push_bool   = push Row.push_bool
let push_string = push Row.push_string


let get_w_name f mname rname party =
  let n = List.find_index (Member.has_name mname) (p_members_dat party) in
  let r = List.find (Row.has_name rname) (p_rows_dat party) in
  f n r

let get_w_id f mid rid party =
  let n = List.find_index (Counted.has_id mid) (p_members party) in
  let r = List.find (Counted.has_id rid) (p_rows party) |> Counted.p_dat in
  f n r
