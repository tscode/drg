
open Ext_std
open Id
open Value

type t =
  | Add_mem  of Member.t
  | Add_row  of Named.t
  | Add_rule of Ion.t list * Rule.t
  | Del_mem  of Ion.t
  | Del_row  of Ion.t
  | Del_rule of Ion.t
  [@@deriving show, yojson]

type pack = t list [@@deriving show, yojson]

let pack l = l

let add_mem a    = Add_mem a
let add_row a    = Add_row a
let add_rule m r = Add_rule (m, r)

let del_mem  ion = Del_mem  ion
let del_row  ion = Del_row  ion
let del_rule ion = Del_rule ion

