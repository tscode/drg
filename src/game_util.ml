
open Ext_std
open Value
open Id
open World


let err_exists a ion = error [a ^ " " ^ Ion.str ion ^ " already exists"]
let err_lacks  a ion = error [a ^ " " ^ Ion.str ion ^ " does not exist"]



module Check = struct

let compability_mod m world = match m with

  | Mod.Add_mem a ->
    if Party.has_member_w_name (Member.p_name a) world.party
    then err_exists "Member" (Ion.name (Member.p_name a)) else ok []

  | Mod.Add_row a ->
    if Party.has_row_w_name (Named.p_name a) world.party
    then err_exists "Row" (Ion.name (Named.p_name a)) else ok []

  | Mod.Add_rule (mids, a) ->
    if Rule.has_rule_w_name (Rule.p_name a) world.rules
    then err_exists "Rule" (Ion.name (Rule.p_name a)) else ok []

  | Mod.Del_mem ion ->
    if Party.has_member_w_ion ion world.party
    then ok [] else err_lacks "Member" ion

  | Mod.Del_row ion ->
    if Party.has_row_w_ion ion world.party
    then ok [] else err_lacks "Row" ion

  | Mod.Del_rule ion ->
    if Rule.has_rule_w_ion ion world.rules
    then ok [] else err_lacks "Rule" ion

let compability_pack pack world =
  List.map (fun m -> compability_mod m world) pack 
  |> List.collect_results
  |> lift_result List.concat List.concat


end

module Resolve = struct

module IonId = struct

let member ion world = 
  let f id = match (Party.has_member_w_id id world.party) with 
    | true -> ok id 
    | false -> err_lacks "Member" ion
  in
  let g name = match (Party.member_id_w_name name world.party) with
    | exception Not_found -> err_lacks "Member" ion
    | id -> ok id
  in
  Ion.lift f g ion
    
let god ion world =
 let f id = match (id = 0) with 
 | true -> ok id 
 | false -> err_lacks "God" ion
 in
 let g name = match Member.has_name name world.god with
 | true -> ok 0
 | false -> err_lacks "God" ion
 in
 Ion.lift f g ion

let row ion world = 
  let f id = match (Party.has_row_w_id id world.party) with
    | true -> ok id
    | false -> err_lacks "Row" ion 
  in
  let g name = match (Party.row_id_w_name name world.party) with
    | id -> ok id
    | exception Not_found -> err_lacks "Row" ion
  in
  Ion.lift f g ion


let rule ion world = 
  let f id = match Rule.has_rule_w_id id world.rules with 
    | true -> ok id
    | false -> err_lacks "Rule" ion
  in
  let g name = match Rule.id_w_rule_name name world.rules with 
    | exception Not_found -> err_lacks "Rule" ion
    | id -> ok id
  in
  Ion.lift f g ion


let augment single ions world =
  let lst = List.map (fun ion -> single ion world) ions in
  match List.separate is_ok lst with
  | oks, [] -> ok (List.map strip_ok oks)
  | _, errs -> error (List.concat (List.map strip_error errs))


let members = augment member
let rows    = augment row
let rules   = augment rule

end

module IdName = struct

let member id world = 
  match Party.member_name_w_id id world.party with
  | name -> ok name
  | exception Not_found -> err_lacks "Member" (Ion.id id)
    
let god id world = match id = 0 with
  | true -> ok (Member.p_name world.god)
  | false -> err_lacks "God" (Ion.id 0)

let row id world =
  match Party.row_name_w_id id world.party with
  | name -> ok name
  | exception Not_found -> err_lacks "Row" (Ion.id id)


let augment single ids world =
  let lst = List.map (fun id -> single id world) ids in
  match List.separate is_ok lst with
  | oks, [] -> ok (List.map strip_ok oks)
  | _, errs -> error (List.concat (List.map strip_error errs))

let members = augment member
let rows = augment row

end

end


module Auth = struct

let authorize_member ion pwd world =
  match Resolve.IonId.member ion world with
  | Error _ -> err_lacks "Member" ion
  | Ok id -> let mem = (Party.member_w_id id world.party) in
    match Member.has_pwd pwd mem with
    | false -> error ["Wrong password for member " ^ Ion.str ion]
    | true  -> ok id


let authorize ion pwd world =
  match Resolve.IonId.god ion world, pwd = Member.p_pwd world.god with
  | Ok _, true -> ok 0
  | Ok _, false -> error ["Wrong password for god " ^ Ion.str ion]
  | Error _, _ -> authorize_member ion pwd world

end


module Censor = struct


let _member id mem = match id = 0 with
  | true -> mem
  | false -> Member.(create (p_name mem) "***")

let _mod id m = match id = 0, m with
  | true, _ -> m
  | false, Mod.Add_mem mem -> Mod.add_mem (_member id mem)
  | false, a -> a

let _pack id p = List.map (_mod id) p

let _event id = function
  | Event.Impl l -> Event.impl (_pack id l)
  | Event.Prop l -> Event.prop (_pack id l)
  | a -> a


let party mid world = 
  let members = Counted.List.mapid 
    (fun id mem -> if id = mid then mem else _member id mem) 
    (Party.p_members world.party) 
  in
  let rows = (Party.p_rows world.party) in
  Party.create ~members ~rows ()
    
let god id world = _member id world.god

let history id n world = 
  let past = match n < 0 with
  | true -> world.past
  | false -> List.filteri (fun i x -> i < n) world.past
  in
  match id = 0 with
  | true -> past
  | false -> Counted.List.map (fun (ev, info) -> (_event id ev, info)) past

let ballots id world = 
  Counted.List.map (Ballot.repack (_pack id)) world.ballots

let world id world = 
  match id = 0 with
  | true -> world
  | false ->
    let god = _member id world.god in
    let party = party id world in
    let ballots = ballots id world in
    (* 
     * History is not censored here 
     * because it can only be retrieved 
     * via the History-query. 
     *)
    { world with god; party; ballots }

end
