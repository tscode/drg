
open Ext_std

(* ids and counters *)
type id = int [@@deriving show, yojson]
type counter = int [@@deriving show, yojson]

let counter () = 0
let incr c = c + 1


(* types wrapped with ids *)
type 'a w_id = {
  id  : id;
  dat : 'a
} [@@deriving show, yojson]

let w_id id dat = { id; dat }

let str id = "#" ^ string_of_int id

let has_id id v = (id = v.id)
let map f l = List.map (fun {id; dat} -> {id; dat = f dat}) l
let rem f l = List.rem (fun {id=_; dat} -> f dat) l
let remi f l = List.remi (fun i {id=_; dat} -> f i dat) l

let map_w_id f l = List.map (fun {id; dat} -> {id; dat = f id dat}) l

let id a = a.id
let dat a = a.dat

let find_id f l = id (List.find (fun {id; dat} -> f dat) l)
let rem_w_id id l = List.rem (has_id id) l
let find_w_id id l = dat (List.find (has_id id) l)
let exists_w_id id l = List.exists (has_id id) l




