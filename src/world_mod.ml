
open Ext_std
open Value

type t =
  
  | Add_row of Named.t
  | Add_rule of Id.id list * Rule.t
  | Add_member of Member.t

  | Del_row of Id.id
  | Del_rule of Id.id
  | Del_member of Id.id

  [@@deriving show, yojson]



let add_row a    = Add_row a
let add_rule m r = Add_rule (m, r)
let add_member a = Add_member a


let del_row id    = Del_row id
let del_rule id   = Del_rule id
let del_member id = Del_member id
