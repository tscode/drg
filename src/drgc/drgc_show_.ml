
open Ext_std
open Id
open Value

open World
open Game

open Drgc_util
open Printf


let fail_lacks head ion = 
  Failure (head ^ " " ^ Ion.str ion ^ " does not exist")
 
let fail_lacks_prop head ion arg =
  Failure (head ^ " " ^ Ion.str ion ^ " does not have property " ^ arg)

let fail_lacks_subprop prop sprop =
  Failure ("Property " ^ prop ^ " does not have sub-property " ^ sprop)

let fail_pattern prop pat =
  Failure ("Selection pattern " ^ pat ^ " of attribute " ^ prop ^ " is invalid")

module PP = struct


let indent str = "  " ^ Bytes.replace "\n" "\n  " str

let date world t = 
  let open Date in
  let t = to_local t in
  sprintf "%02d.%02d.%02d %02d:%02d:%02d" t.day t.month t.year t.h t.m t.s


let god world god = 
  let fmt = format_of_string "God: %s\nPassword: %s" in
  sprintf fmt (Member.p_name god) (Member.p_pwd god)


let rule_name world = Rule.p_name
let rule_descr world = Rule.p_descr
let rule_cat world = Rule.p_cat
let rule_spec world sp = "Warning: Spec not implemented yet"
let rule_prog world pr = Rule.Prog.show pr
let rule_targets world ions = List.map (
    fun ion -> try Party.member_name_w_ion ion world.party with
    | Not_found -> ""
    ) ions
    |> List.filter ((<>) "")
    |> Bytes.concat "\n"

let rule_overview world rule info =
  let fmt = format_of_string
    "Name: %s\nDescription: %s\nCategory: %s\nTargets: %s\nDate: %s"
  in
  let targets = 
    rule_targets world (List.map Ion.id (Rule.p_ids info))
    |> Bytes.replace "\n" ", "
  in
  sprintf fmt 
    (rule_name world rule)
    (rule_descr world rule)
    (rule_cat world rule)
    targets
    (date world (Rule.p_date info))
    
let rules_overview world rules =
  let fmt = format_of_string
    "#%d %s: %s"
  in
  Counted.List.mapid (
    fun id (rule, info) -> 
      sprintf fmt id 
      (rule_name world rule)
      (rule_descr world rule)
  ) world.rules
  |> List.map Counted.p_dat
  |> String.concat "\n"



let member_name world = Member.p_name
let member_pwd world = Member.p_pwd

let member world cmem = 
  let id = Counted.p_id cmem in
  let mem = Counted.p_dat cmem in
  let party = p_party world in
  let m = Member.p_name mem in
  let name_str = match Member.p_pwd mem with
  | "***" -> sprintf "Member %s with id #%d\nAttributes:\n" m id
  | pwd   -> sprintf "Member %s with id #%d\nPassword: %s\nAttributes:\n" m id pwd
  in
  let f r = 
    let raw = Party.get_w_name Row.get_raw m r party in
    sprintf "  %s: %s" r (Raw.to_string raw)
  in
  let attr_str = 
    List.map f (Party.p_rows_name party)
    |> Bytes.concat "\n"
  in
  name_str ^ attr_str

let member_attrib world = Raw.to_string



let attrib_name world = Row.p_name
let attrib_default world = Raw.to_string $ Row.p_default

let attrib world crow =
  let id = Counted.p_id crow in
  let row = Counted.p_dat crow in
  let party = p_party world in
  let r = attrib_name world row in
  let d = attrib_default world row in
  let name_str = 
    sprintf "Attribute %s with id #%d\nDefault: %s\nCurrent values:\n" r id d 
  in
  let f m =
    let raw = Party.get_w_name Row.get_raw m r party in
    sprintf "  %s: %s" m (Raw.to_string raw)
  in
  let mem_str =
    List.map f (Party.p_members_name party)
    |> Bytes.concat "\n"
  in
  name_str ^ mem_str


let members_overview world members =
  let f cmem =
    let id = Counted.p_id cmem in
    let mem = Counted.p_dat cmem in
    let name = member_name world mem in
    match member_pwd world mem with
    | "***" -> sprintf "#%d %s" id name
    | pwd   -> sprintf "#%d %s (%s)" id name pwd
  in
  List.map f (List.rev members)
  |> Bytes.concat "\n"

let attribs_overview world rows =
  let f crow =
    let id = Counted.p_id crow in
    let row = Counted.p_dat crow in
    let name = attrib_name world row in
    let default = attrib_default world row in
    sprintf "#%d %s (default: %s)" id name default
  in
  List.map f (List.rev rows)
  |> Bytes.concat "\n"


let party_overview world party =
  let members = members_overview world (Party.p_members party) in
  let attribs = attribs_overview world (Party.p_rows party) in
  sprintf "Members:\n%s\nAttributes:\n%s" 
    (indent members) (indent attribs)


let member_attrib_trend world dates strs =
  let date_strs = List.map (date world) dates in
  List.map2 (sprintf "%s: %s") date_strs strs
  |> Bytes.concat "\n"


let mod_short world = function
  | Mod.Add_mem mem -> sprintf "+m %s" (member_name world mem)
  | Mod.Del_mem ion -> sprintf "-m %s" (Ion.str ion)
  | Mod.Add_row nmd -> sprintf "+a %s" (Named.p_name nmd)
  | Mod.Del_row ion -> sprintf "-a %s" (Ion.str ion)
  | Mod.Add_rule (_,rule) -> sprintf "+r %s" (rule_name world rule)
  | Mod.Del_rule ion  -> sprintf "-r %s" (Ion.str ion)


let mod_norm world = function
  | Mod.Add_mem mem -> sprintf "Add member %s" (member_name world mem)
  | Mod.Del_mem ion -> sprintf "Delete member %s" (Ion.str ion)
  | Mod.Add_row nmd -> sprintf "Add attrib %s" (Named.p_name nmd)
  | Mod.Del_row ion -> sprintf "Delete attrib %s" (Ion.str ion)
  | Mod.Del_rule ion  -> sprintf "Delete rule %s" (Ion.str ion)
  | Mod.Add_rule (_,rule) -> sprintf "Add rule %s" (rule_name world rule)

let mod_full world = function
  | Mod.Add_mem mem -> sprintf "Add member %s" (member_name world mem)
  | Mod.Del_mem ion -> sprintf "Delete member %s" (Ion.str ion)
  | Mod.Add_row nmd -> sprintf "Add attrib %s" (Named.p_name nmd)
  | Mod.Del_row ion -> sprintf "Delete attrib %s" (Ion.str ion)
  | Mod.Del_rule ion  -> sprintf "Delete rule %s" (Ion.str ion)
  | Mod.Add_rule (ions,rule) -> 
  let fmt = format_of_string
    "Add Rule %s\n  Description: %s\n  Category: %s\n  Targets: %s"
  in
  sprintf fmt 
    (rule_name world rule)
    (rule_descr world rule)
    (rule_cat world rule)
    (rule_targets world ions)


let ballot_overview world ballots =
  let f cbal =
    let id = Counted.p_id cbal in
    let bal = Counted.p_dat cbal in
    let pack = Ballot.p_pack bal in
    let yes, no = Ballot.yesno bal in
    let mods = 
      List.map (mod_short world) (List.take pack 3)
      |> Bytes.concat ", " 
    in
    sprintf "#%d %s (%d:%d)" id mods yes no
  in
  List.map f (List.rev ballots)
  |> Bytes.concat "\n"

let ballot_yes world status =
  let ids = List.filter snd status
    |> List.map fst
  in
  match Util.Resolve.IdName.members ids world with
  | Error _ -> raise (Failure "Unexpected: Member that voted is not in party")
  | Ok names  -> match names with
    | [] -> "-"
    | _  -> Bytes.concat "\n" names

let ballot_no world status =
  let ids = List.filter (not $ snd) status
    |> List.map fst
  in
  match Util.Resolve.IdName.members ids world with
  | Error _ -> raise (Failure "Unexpected: Member that voted is not in party")
  | Ok names  -> match names with
    | [] -> "-"
    | _  -> Bytes.concat "\n" names

let ballot_status world status =
  let yes = ballot_yes world status in
  let no  = ballot_no world status in
  sprintf "Yes:\n%s\nNo:\n%s" (indent yes) (indent no)


let ballot_mods world pack =
  List.map (mod_norm world) pack |> Bytes.concat "\n" 

let ballot world cbal =
  let id = Counted.p_id cbal in
  let bal = Counted.p_dat cbal in
  let pack = Ballot.p_pack bal in
  let status = Ballot.p_status bal in
  let mods = 
    List.map (mod_norm world) pack
    |> Bytes.concat "  \n" 
  in
  sprintf "Ballot with id #%d\n%s\nMods:\n  %s" 
    id (ballot_status world status) mods


end


let show_name world = function
  | [] -> p_name world
  | h :: _ -> raise (fail_lacks_subprop "name" h)

let show_god world = function
  | [] -> PP.god world (p_god world)
  | "name" :: [] -> Member.p_name (p_god world)
  | "pwd" :: [] -> Member.p_pwd (p_god world)
  | h :: _ -> raise (fail_lacks_subprop "god" h)

let show_rules world = function
  | [] -> PP.rules_overview world world.rules
  | ion :: tl -> 
    let ion = Ion.of_string ion in
    match Rule.entry_w_ion ion world.rules with
    | exception Not_found -> raise (fail_lacks "Rule" ion) 
    | rule, info -> match tl with
      | [] -> PP.rule_overview world rule info
      | ["name"]    -> PP.rule_name world rule
      | ["descr"]   -> PP.rule_descr world rule
      | ["cat"]     -> PP.rule_cat world rule
      | ["spec"]    -> PP.rule_spec world rule
      | ["prog"]    -> PP.rule_prog world (Rule.p_program rule)
      | ["date"]    -> PP.date world (Rule.p_date info)
      | ["targets"] -> PP.rule_targets world (List.map Ion.id (Rule.p_ids info))
      | h :: [] -> raise (fail_lacks_prop "Rule" ion h)
      | h :: t  -> raise (fail_lacks_subprop h (List.hd t))


let extract_index pat str =
  let l = Bytes.length pat in
  let k = Bytes.length str in
  match Bytes.sub str 0 l = pat with
  | exception Invalid_argument _ -> `Wrong_property
  | false -> `Wrong_property
  | true ->
    let index_str = Bytes.sub str l (k - l) in
    match int_of_string index_str with
    | i -> `Ok i
    | exception Failure _ -> `Wrong_index index_str


let extract_slice pat str max =
  let l = Bytes.length pat in
  let k = Bytes.length str in
  match Bytes.sub str 0 l = pat with
  | exception Invalid_argument _ -> `Wrong_property
  | false -> `Wrong_property
  | true  -> 
    let slice_str = Bytes.sub str l (k - l) in
    match int_of_string slice_str with
    | i -> `Ok (i, i+1)
    | exception Failure _ ->
      try
      match String.split_on_char ':' slice_str with
      | [""] | [""; ""] -> `Ok (0, max)
      | [""; b] -> `Ok (0, int_of_string b)
      | [a; ""] -> `Ok (int_of_string a, max)
      | [a; b]  -> `Ok (int_of_string a, int_of_string b)
      | _ -> `Wrong_pattern slice_str
      with
      | Invalid_argument _ | Failure _ -> `Wrong_pattern slice_str
    

let show_members world = function
  | [] -> PP.members_overview world (Party.p_members world.party)
  | ion :: tl ->
    let mion = Ion.of_string ion in
    match Party.counted_member_w_ion mion world.party with
    | exception Not_found -> raise (fail_lacks "Member" mion)
    | cmem -> 
      let mem = Counted.p_dat cmem in
      match tl with
      | [] -> PP.member world cmem
      | ["name"] -> PP.member_name world mem
      | ["pwd"]  -> PP.member_pwd world mem
      | "name" :: tl -> raise (fail_lacks_subprop "name" (List.hd tl))
      | "pwd" :: tl -> raise (fail_lacks_subprop "pwd" (List.hd tl))
      | ion :: tl ->
        let rion = Ion.of_string ion in
        match Party.row_w_ion rion world.party with
        | exception Not_found -> raise (fail_lacks "Row" rion)
        | row -> 
          let mname = Member.p_name mem in
          let rname = Row.p_name row in
          match tl with
          | [] ->
            let raw = Party.get_w_name Row.get_raw mname rname world.party in 
            PP.member_attrib world raw
          | ["name"] -> PP.attrib_name world row
          | ["def"]  -> PP.attrib_default world row
          | h :: tl ->
            let dates = Party.get_w_name Row.get_dates mname rname world.party in
            let cell = Party.get_w_name Row.get_cell mname rname world.party in
            let strs = Cell.to_string_list cell in
            let len  = List.length strs in
            match extract_slice "trend" h len with
            | `Wrong_property  -> raise (fail_lacks_prop "Row" rion h)
            | `Wrong_pattern p -> raise (fail_pattern "trend" p)
            | `Ok (a, b) -> 
              if tl <> [] 
              then raise (fail_lacks_subprop "trend" (List.hd tl))
              else
              let strs  = List.sub strs  a (b - a) in
              let dates = List.sub dates a (b - a) in
              PP.member_attrib_trend world dates strs 


let show_attribs world = function
  | [] -> PP.attribs_overview world (Party.p_rows world.party)
  | ion :: tl ->
    let ion = Ion.of_string ion in
    match Party.counted_row_w_ion ion world.party with
    | exception Not_found -> raise (fail_lacks "Row" ion)
    | crow ->
      let row = Counted.p_dat crow in
      match tl with
      | [] -> PP.attrib world crow
      | ["name"] -> PP.attrib_name world row
      | ["def"]  -> PP.attrib_default world row
      | h :: _ -> raise (fail_lacks_prop "Row" ion h)


let show_party world = function
  | [] -> PP.party_overview world world.party
  | "members" :: t -> show_members world t
  | "attribs" :: t -> show_attribs world t
  | h :: _ -> raise (fail_lacks_subprop "party" h)


let show_ballots world = function
  | [] -> PP.ballot_overview world world.ballots
  | id :: tl -> 
    match int_of_string id with
    | exception Failure _ -> raise ( Failure
      (sprintf "%s does not correspond to a ballot. Must be a valid id." id)
      )
    | id ->
      match Ballot.counted_ballot_w_id id world.ballots with
      | exception Not_found -> raise (fail_lacks "Ballot" (Ion.id id))
      | cbal ->
        let bal = Counted.p_dat cbal in
        let status = Ballot.p_status bal in
        let pack = Ballot.p_pack bal in
        match tl with
        | [] -> PP.ballot world cbal
        | ["status"] -> PP.ballot_status world status
        | ["yes"] -> PP.ballot_yes world status
        | ["no"] -> PP.ballot_no world status
        | ["mods"] -> PP.ballot_mods world pack
        | h :: tl ->
          match extract_index "mod" h with
          | `Wrong_property -> raise (fail_lacks_prop "Ballot" (Ion.id id) h)
          | `Wrong_index  p -> raise (fail_pattern "mod" p)
          | `Ok i ->
            match tl with
            | h :: _ -> raise (fail_lacks_subprop "mod" h)
            | [] -> match List.nth pack i with
              | exception Failure _ -> raise (fail_pattern "mod" (string_of_int i))
              | m -> PP.mod_full world m
  


let show_direct world ion l =
  if Ion.lift ((=) 0) ((=) (Member.p_name world.god)) ion then
  show_god world l
  else if Party.has_member_w_ion ion world.party then
  show_party world ("members" :: l)
  else if Party.has_row_w_ion ion world.party then
  show_party world ("attribs" :: l)
  else if Rule.has_rule_w_ion ion world.rules then
  show_rules world l
  else match ion with
  | Ion.Name name -> raise (Failure ("Unknown property or name " ^ name))
  | Ion.Id id ->
    if Ballot.has_ballot_w_id id world.ballots then
    show_ballots world l
    else raise (Failure ("Unknown id " ^ string_of_int id))



let show_world world = 
  sprintf "World name: %s\nGod: %s\n%s\nRules:\n%s\nBallots:\n%s" 
    (show_name world [])
    (show_god world ["name"])
    (show_party world [])
    (show_rules world [] |> PP.indent)
    (show_ballots world [] |> PP.indent)


let show_sel world = function
  | [] -> show_world world
  | "name" :: t -> show_name world t
  | "god"  :: t -> show_god world t
  | "party" :: t -> show_party world t
  | "rules" :: t -> show_rules world t
  | "ballots" :: t -> show_ballots world t
  | "history" :: t -> raise (Failure "Not implemented yet")

  | "members" :: t -> show_party world ("members" :: t)
  | "attribs" :: t -> show_party world ("attribs" :: t)

  | ion :: t -> show_direct world (Ion.of_string ion) (ion :: t)


let body sel conn =
  let f world = match show_sel world sel with
    | exception Failure err -> error err
    | disp -> print_string disp; flush Pervasives.stdout; ok (ok [])
  in
  construct_body "" (Client.w_world f) (conn ())

let show sel conn = run_body (body sel) conn


open Cmdliner

let sel =
  let doc = "Selector of the part of the world to be shown" in
  Arg.(value & pos 0 (list ~sep:'.' string) []
             & info [] ~docv:"SELECTOR" ~doc)

let show_t =
  let doc = "display information about the drg world" in
  let exits = Term.default_exits in
  let man = [
    `S Manpage.s_description;
    `P "This command can be used to get information about the world. This
    is only a preliminary implementation of the show command of future
    releases.";
    `Blocks help_secs
  ] in
  Term.(ret (const show $ sel $ Drgc_login.login_t)),
  Term.(info "show" ~doc ~sdocs:Manpage.s_common_options ~exits ~man)

