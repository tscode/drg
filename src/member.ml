
type t = { name : bytes; pwd : bytes } [@@deriving show, yojson]

let create name pwd = { name; pwd }

let name member = member.name
let pwd member = member.pwd

let has_name mname member = (name member = mname)
let has_pwd p member = (pwd member = p)

let censor mem = { mem with pwd = "***" }
