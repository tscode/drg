
type t =
  | Prop of World_mod.t list
  | Vote of Id.id * bool
  | Impl of World_mod.t list
  | Act  of Id.id * Rule.Form.t
  [@@deriving show, yojson]


let prop pack = Prop pack
let vote id b = Vote (id, b)
let impl pack = Impl pack
let act id form = Act (id, form)
