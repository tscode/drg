
open Ext_std


type id = int [@@deriving show, yojson]

let id_to_str id = "#" ^ string_of_int id

module Counted = struct

type 'a t = {
  id  : id;
  dat : 'a
} [@@deriving show, yojson]

type 'a clist = 'a t list [@@deriving show, yojson]

let create id dat = { id; dat }

let p_id a = a.id
let p_dat a = a.dat

let has_id  id  a = (id = p_id a)
let has_dat dat a = (dat = p_dat a)


module List = struct

type 'a t = 'a clist

let map  f l = List.map (fun {id; dat} -> {id; dat = f dat}) l
let rem  f l = List.rem (fun {id=_; dat} -> f dat) l
let find f l = List.find (fun {id=_; dat} -> f dat) l

let mapi  f l = List.mapi  (fun i {id; dat} -> {id; dat = f i dat}) l
let remi  f l = List.remi  (fun i {id=_; dat} -> f i dat) l
let findi f l = List.findi (fun i {id=_; dat} -> f i dat) l

let mapid  f l = List.map  (fun {id; dat} -> {id; dat = f id dat}) l
let remid  f l = List.rem  (fun {id; dat} -> f id dat) l
let findid f l = List.find (fun {id; dat} -> f id dat) l

let find_index f l = List.find_index (fun {id=_; dat} -> f dat) l

end

end


module Counter = struct

type t = int [@@deriving show, yojson]

let create () = 1
let incr c = c + 1

end


module Ion = struct

type t =
  | Id of id
  | Name of bytes
  [@@deriving show, yojson]


let id a = Id a
let name a = Name a

let lift f g = function
  | Id id     -> f id
  | Name name -> g name

let str = function
  | Id id -> id_to_str id
  | Name name -> name

let is_id = function
  | Id _ -> true
  | Name _ -> false

let is_name = function
  | Id _ -> false
  | Name _ -> true

let of_string str =
  try id (int_of_string str) with
  | Failure _ -> name str

end


