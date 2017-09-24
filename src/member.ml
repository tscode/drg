
type t = { 
  name : bytes; 
  pwd : bytes 
} [@@deriving show, yojson]

let create name pwd = { name; pwd }
(*let censor mem = { mem with pwd = "***" }*)

let p_name member = member.name
let p_pwd  member = member.pwd

let has_name mname member = (p_name member = mname)
let has_pwd pwd member    = (p_pwd member  = pwd)

