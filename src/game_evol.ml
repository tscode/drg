

open Ext_std
open Value
open Id
open World

open Game_util


let initial_info aid aname =
  { aid; aname; fail=false; date=Date.now (); msg=[] }


let register_event (ev, info) world = { world with
  past = Counted.create world.count (ev, info) :: world.past;
  count = world.count + 1
}

let finish info world = match info.fail with
  | false -> (world, ok    info.msg)
  | true  -> (world, error info.msg)



module Impl = struct

let add_row name raw world = match Party.{ world with
  party = add_row Row.of_raw world.count name raw world.party;
  count = world.count + 1
  } with
  | exception Exists -> err_exists "Row" (Ion.name name)
  | world -> ok world

let del_row ion world = match Party.{ world with
  party = del_row_w_ion ion world.party
  } with
  | exception Not_found -> err_lacks "Row" ion
  | world -> ok world


let add_mem mem world = match Party.{ world with
  party =
    Member.(add_member world.count (p_name mem) (p_pwd mem) world.party);
  count = world.count + 1
  } with
  | exception Exists -> err_exists "Member" (Ion.name (Member.p_name mem))
  | world -> ok world

let del_mem ion world = match Party.{ world with
  party = del_member_w_ion ion world.party
  } with
  | exception Not_found -> err_lacks "Member" ion
  | world -> ok world

let add_rule ions rule world =
    match Resolve.IonId.members ions world with
    | Error msgs -> error msgs
    | Ok ids -> match Party.{ world with
      rules = Rule.add_rule world.count ids rule world.rules;
      count = world.count + 1
      } with
      | exception Exists -> err_exists "Rule" (Ion.name (Rule.p_name rule))
      | world -> ok world

let del_rule ion world = match Rule.{ world with
  rules = del_rule_w_ion ion world.rules
  } with
  | exception Not_found -> err_lacks "Rule" ion
  | world -> ok world


let implement_mod m world = match m with

  | Mod.Add_mem  mem   -> add_mem mem world
  | Mod.Add_row  nv    -> add_row (Named.p_name nv) (Named.p_raw nv) world
  | Mod.Add_rule (i,r) -> add_rule i r world

  | Mod.Del_mem  ion -> del_mem ion world
  | Mod.Del_row  ion -> del_row ion world
  | Mod.Del_rule ion -> del_rule ion world


let rec implement_pack pack fail msgs world = match pack with
  | [] -> if fail then Error (List.rev msgs) else Ok world
  | h :: t -> match implement_mod h world with
    | Ok world  -> implement_pack t fail msgs world
    | Error msg -> implement_pack t true (msg @ msgs) world 

let handle info pack world =
  let fail, msg, world = 
    match info.aid = 0, implement_pack pack false [] world with
    | false, _ -> true, ["Only god can implement modifications"], world
    | true, Error m -> true, m, world
    | true, Ok w -> false, [], w
    in
  let info = { info with fail; msg } in
  world
  |> register_event (Event.impl pack, info)
  |> finish info

end


module Vote = struct

let err_lacks_msg id = "Ballot " ^ Ion.str (Ion.id id) ^ " does not exist"
let accept_msg id = "Ballot " ^ Ion.str (Ion.id id) ^ " was accepted"
let reject_msg id = "Ballot " ^ Ion.str (Ion.id id) ^ " was rejected"


let handle info bid flag world =

  let num_mems = Party.num_members world.party in
  match Ballot.update_box num_mems info.aid flag bid world.ballots with

  | exception Not_found ->
    let info = { info with fail = true; msg = [err_lacks_msg bid] } in
    (register_event (Event.vote bid flag, info) world)
    |> finish info

  | None, ballots, _ ->
    register_event (Event.vote bid flag, info) { world with ballots }
    |> finish info

  | Some false, ballots, _ ->
    let info = { info with fail = false; msg = [reject_msg bid] } in
    register_event (Event.vote bid flag, info) { world with ballots }
    |> finish info

  | Some true, ballots, pack ->
    let info = { info with fail = false; msg = [accept_msg bid] } in
    let impl_info = (initial_info (0) (Member.p_name world.god)) in
    let world = 
      register_event (Event.vote bid flag, info) { world with ballots }
    in
    match Impl.handle impl_info pack world with
    | world, Ok msgs    -> world, Ok (info.msg @ msgs)
    | world, Error msgs -> world, Ok (info.msg @ msgs)

end


module Prop = struct

let register_ballot pack world = { world with
  ballots = Ballot.add_ballot world.count pack world.ballots;
  count = world.count + 1
}

let handle info pack world =
  match Check.compability_pack pack world with

  | Error msg ->
    let info = { info with fail = true; msg } in
    world
    |> register_event (Event.prop pack, info)
    |> finish info

  | Ok msg ->
    let info  = { info with fail = false; msg } in 
    world
    |> register_ballot pack
    |> register_event (Event.prop pack, info)
    |> finish info


end


module Act = struct

let add_defaults info form = form
  |> Rule.Form.write_raw "self" (Raw.string info.aname)
  |> Rule.Form.write_raw "date" (Raw.float info.date)

let load_program info ion world =
  match Resolve.IonId.rule ion world with
  | Error msg -> error msg
  | Ok id ->
    let rule, meta = Rule.entry_w_id id world.rules in
    match List.mem info.aid (Rule.p_ids meta) with
    | true  -> ok (Rule.p_program rule)
    | false -> error
      ["Rule " ^ Ion.str ion ^ " does not apply to member " ^ info.aname]

let handle info ion form world =
  let world, info = begin 
  match load_program info ion world with
  | Error msg -> world, { info with fail = true; msg }
  | Ok prog ->
    let form = add_defaults info form in
    match Rule.Prog.exec form prog world.party with
    | Error msg -> (world, { info with fail = true; msg })
    | Ok party -> { world with party }, { info with fail = false }
  end 
  in
  world |> register_event (Event.act ion form, info) |> finish info

end


let lookup id world = try match id with
  | 0 -> ok (0, Member.p_name world.god)
  | id -> ok (id, Party.member_name_w_id id world.party)
  with
  | Not_found -> err_lacks "Member" (Ion.id id)


let evolve id event world =
  match lookup id world with
  | Error _ as err -> world, err
  | Ok (aid, aname) ->
    let info = initial_info aid aname in
    match event with
    | Event.Prop pack -> Prop.handle info pack world
    | Event.Impl pack -> Impl.handle info pack world
    | Event.Vote (id, flag) -> Vote.handle info id flag world
    | Event.Act  (id, form) -> Act.handle  info id form world



