
let pt n = print_endline ("test" ^ string_of_int n)

open Ext_std
open Id

type status = (id * bool) list [@@deriving show, yojson]

type t      = {
  pack : World_mod.pack;
  status : status 
} [@@deriving show, yojson]

let p_pack ballot = ballot.pack
let p_status ballot = ballot.status

let create pack = { pack; status = [] }

let yesno ballot =
  let yes, no = List.separate snd (p_status ballot) in
  List.(length yes, length no)


type box = t Counted.clist [@@deriving show, yojson]

let repack f bal = { bal with pack = f bal.pack }

let cast id flag ballot = { ballot with 
  status = (id, flag) :: List.remove_assoc id ballot.status
}

let del_ballot_w_id bid box = 
  Counted.List.remid (fun id _ -> id = bid) box

let add_ballot id pack box =
  Counted.create id (create pack) :: box

let modify_ballot id ballot box =
  Counted.List.mapid (fun i b -> if i = id then ballot else b) box

let has_ballot_w_id bid box =
  match Counted.List.findid (fun id _ -> id = bid) box with
  | exception Not_found -> false
  | _ -> true

let counted_ballot_w_id bid box =
  Counted.List.findid (fun id _ -> id = bid) box

let update_box n id flag bid box =
  let ballot = Counted.List.findid (fun id _ -> id = bid) box in
  pt (List.length (Counted.p_dat ballot).status);
  let ballot = cast id flag (Counted.p_dat ballot) in
  pt (List.length ballot.status);

  let yes, no = List.separate (fun (_, flag) -> flag) ballot.status in
  match List.length yes = n, List.length no = n with
  | true, _ -> Some true,  del_ballot_w_id bid box, ballot.pack
  | _, true -> Some false, del_ballot_w_id bid box, ballot.pack
  | _, _    -> None, modify_ballot bid ballot box,   ballot.pack

