
open Ext_std
open Value
open Id

type t =
  | Prop of World_mod.t list
  | Impl of World_mod.t list
  | Vote of id * bool
  | Act  of Ion.t * Rule.Form.t
  [@@deriving show, yojson]


let prop pack    = Prop pack
let vote id b    = Vote (id, b)
let impl pack    = Impl pack
let act ion form = Act (ion, form)
