

(* Problems:
 * * Votes can not be rejected at the moment
 * * Acts not implemented
 *)

open Ext_std
open Value
open World


let event_info aid world =
  let aname = Member.name
    (if aid = -1 
    then world.god
    else Party.member_w_id aid world.party)
  in
  let date = Date.now () in
  { aid; aname; fail=false; date; msg=[]}


let register_event (ev, info) world = { 
  world with 
  past  = Id.w_id world.count (Game_auth.obscure_event ev, info) :: world.past; 
  count = world.count + 1 
}


let finish info world = match info.fail with
  | true  -> error (world, info.msg)
  | false -> ok    (world, info.msg)


let err_nex a id  = error (a ^ " " ^ Id.str id ^ "does not exist")
let err_ex a name = error (a ^ " with name '" ^ name ^ "' already exists")


module Impl = struct

  let add_row name raw world = match Party.{ 
    world with 
    party = add_row Row.of_raw ~id:world.count name raw world.party;
    count = world.count + 1
    } with
    | exception Party.Party_error -> err_ex "Row" name
    | world -> ok world
  
  let del_row rid world = match Party.{
    world with
    party = del_row_w_id rid world.party
    } with
    | exception Party.Party_error -> err_nex "Row" rid
    | world -> ok world
  
  let add_member member world = match Party.{
    world with
    party = add_member ~id:world.count (Member.name member) (Member.pwd member) world.party;
    count = world.count + 1
    } with
    | exception Party.Party_error -> err_ex "Member" (Member.name member)
    | world -> ok world
  
  let del_member mid world = match Party.{
    world with
    party = del_member_w_id mid world.party
    } with
    | world -> ok world
    | exception Party.Party_error -> err_nex "Member" mid
  
  let add_rule mids rule world = 
    let info = { mems = mids; date = Date.now () } in
    match {
    world with 
    rules = Id.w_id world.count (rule, info) :: world.rules;
    count = world.count + 1
    } with
    | world -> ok world
    (* TODO: check if rule with same name exists! *)
  
  let del_rule rid world = ok {
    world with
    rules = Id.rem_w_id rid world.rules;
  }
  
  let implement_mod m world = match m with
    | Mod.Add_row    named        -> add_row (Named.name named) (Named.to_raw named) world
    | Mod.Del_row    rid          -> del_row rid world
    | Mod.Add_member mem          -> add_member mem world
    | Mod.Del_member mid          -> del_member mid world
    | Mod.Add_rule   (mids, rule) -> add_rule mids rule world
    | Mod.Del_rule   rid          -> del_rule rid world
  
  
  let rec implement_pack pack fail cmsg cworld = match pack with
    | [] -> if fail then Error cmsg else Ok cworld
    | h :: t -> match implement_mod h cworld with
      | Ok w -> implement_pack t fail cmsg w
      | Error msg -> implement_pack t true (msg :: cmsg) cworld
  
  
  let handle info pack world =
    let fail, msg, world = match implement_pack pack false [] world with
    | Ok w -> false, [], w
    | Error m -> true, m, world
    in
    let info = { info with fail; msg } in
    world
    |> register_event (Event.impl pack, info)
    |> finish info


end


module Vote = struct

  let get_status bid world =
    match Id.find_w_id bid world.ballots with
    | exception Not_found -> err_nex "Ballot" bid
    | pack, status -> ok (pack, status)


  let conduct_vote info bid flag status world =
    let m = List.length (Party.members world.party) in
    let status = (info.aid, flag) :: List.remove_assoc info.aid status in
    let yes, no = List.fold_left (fun (y,n) (_, b) -> 
      if b then (y + 1, n) else (y, n + 1) 
    ) (0, 0) status
    in
    match yes = m, no = m with
    | true, _ -> 
      let world = { world with ballots = Id.rem_w_id bid world.ballots } in
      let msg   = "Ballot " ^ Id.str bid ^ " was accepted" in
      (world, msg, true)
    | _, true ->
      let world = { world with ballots = Id.rem_w_id bid world.ballots } in
      let msg   = "Ballot " ^ Id.str bid ^ " was rejected" in
      (world, msg, false)
    | _ -> 
      let ballots = Id.map_w_id (fun id (pack, b) ->
        if id = bid then (pack, status) else (pack, b)
      ) world.ballots
      in ({ world with ballots }, "", false)


  let handle info bid flag world =
    match get_status bid world with
    | Error msg ->
      let info = { info with fail=true; msg=[msg] } in
      finish info (register_event (Event.vote bid flag, info) world)
    | Ok (pack, status) ->
      let world, msg, accepted = conduct_vote info bid flag status world in
      let info = { info with fail=false; msg=[msg] } in
      let world = register_event (Event.vote bid flag, info) world
      in 
      if accepted 
      then Impl.handle (event_info (-1) world) pack world 
      else finish info world 
  
end


module Prop = struct

  let unpack a = Id.dat a |> fst
  
  let check_prop world = function
    | Mod.Add_row a -> 
      if Party.has_row (Named.name a) world.party
      then err_ex "Row" (Named.name a)
      else ok ""
    | Mod.Add_rule (mids, a) -> 
      if List.exists 
      (fun x -> unpack x |> Rule.has_name (Rule.name a)) world.rules
      then err_ex "Rule" (Rule.name a)
      else ok ""
    | Mod.Add_member a -> 
      if Party.has_member (Member.name a) world.party
      then err_ex "Member" (Member.name a)
      else ok ""
    | Mod.Del_row id -> 
      if Party.has_row_w_id id world.party
      then ok ""
      else err_nex "Row" id
    | Mod.Del_rule id -> 
      if List.exists (Id.has_id id) world.rules
      then ok ""
      else err_nex "Rule" id
    | Mod.Del_member id -> 
      if Party.has_member_w_id id world.party
      then ok ""
      else err_nex "Member" id
  
  
  let register_ballot pack world = { 
    world with 
    ballots = Id.w_id world.count (pack, []) :: world.ballots;
    count = world.count + 1
  }
  
  let handle info pack world = 
    let res = List.map (check_prop world) pack in
    let msg = List.map (function Error a -> a | Ok a -> a) res in
    let fail = List.exists (function Error _ -> true | Ok _ -> false) res in
    let info = { info with fail; msg } in
    world
    |> register_event (Event.Prop pack, info) 
    |> begin if not fail 
       then register_ballot pack 
       else identity
       end
    |> finish info
  
end


module Act = struct

  let add_defaults info form = form
    |> Rule.Form.write_raw "self" (Raw.string info.aname)
    |> Rule.Form.write_raw "date" (Raw.float info.date)

  let get_program info rid world =
    match Id.find_w_id rid world.rules with
    | exception Not_found -> err_nex "Rule" rid
    | rule, {mems; date=_} -> 
      match List.mem info.aid mems with
      | false -> error
        ("Rule " ^ Id.str rid ^ " does not apply to member " ^ Id.str info.aid)
      | true -> ok (Rule.program rule)

  let handle info rid form world =
    let form = add_defaults info form in
    let world = match get_program info rid world with
    | Error msg -> 
      let info = { info with fail=true; msg=[msg] } in
      register_event (Event.Act (rid, form), info) world
    | Ok program ->
      match Rule.Program.exec form program world.party with
      | Error msg ->
        let info = { info with fail=true; msg=[msg] } in
        register_event (Event.Act (rid, form), info) world
      | Ok party ->
        let info = { info with fail=false } in
        { world with party }
        |> register_event (Event.Act (rid, form), info)
    in
    finish info world

end


let authorize aid pwd event world = 
  match aid = -1, pwd = Member.pwd world.god with
  | true, true  -> None
  | true, false -> Some ("Wrong password for !" ^ (Member.name world.god))
  | false, _    -> match event with
    | Event.Impl _ -> Some "Only god can implement modifications"
    | _ -> 
      let a = Party.has_member_w_id aid world.party in
      let b = Party.is_authorized_w_id aid pwd world.party in
      match a, b with
      | true, true  -> None
      | true, false -> Some ("Wrong password for member " ^ Id.str aid)
      | false, _    -> Some ("Member " ^ Id.str aid ^ " does not exist")


let evolve aid pwd event world =
  match authorize aid pwd event world with
  | Some msg -> error (world, [msg])
  | None ->
    let info = event_info aid world in
    match event with
    | Event.Prop pack    -> Prop.handle info pack world 
    | Event.Vote (id, f) -> Vote.handle info id f world
    | Event.Impl pack    -> Impl.handle info pack world
    | Event.Act (id, f)  -> Act.handle  info id f world


exception Event_error of bytes

let evolve_exn aid pwd event world =
  match evolve aid pwd event world with
  | Ok (w, _) -> w
  | Error (w, msg) -> raise (Event_error (Bytes.concat "\n" msg))

