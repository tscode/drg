
open Ext_std
open Value
open World
open Game

let resolve_ion_member ion world = match ion with
  | Auth.Id id -> begin
    match (Party.has_member_w_id id world.party) with 
    | true -> ok id 
    | false -> error ("Member " ^ Id.str id ^ " does not exist")
    end
  | Auth.Name name -> begin
    match (Party.id_w_name name world.party) with
    | id -> ok id
    | exception Party.Party_error ->
      error ("Member with name '" ^ name ^ "' does not exist")
    end

let resolve_ion_row ion world = match ion with
  | Auth.Id id -> begin
    match (Party.has_row_w_id id world.party) with
    | true -> ok id
    | false -> error ("Row " ^ Id.str id ^ " does not exist")
    end
  | Auth.Name name -> begin
    match (Party.id_w_row name world.party) with
    | id -> ok id
    | exception Party.Party_error ->
      error ("Row with name '" ^ name ^ "' does not exist")
    end


let resolve_ion_rule ion world = match ion with
  | Auth.Id id -> begin
    match (List.exists (Id.has_id id) world.rules) with
    | true -> ok id
    | false -> error ("Rule " ^ Id.str id ^ " does not exist")
    end
  | Auth.Name name -> begin
    match (Id.find_id (fun (rule, _) -> Rule.has_name name rule) world.rules) with
    | id -> ok id
    | exception Not_found ->
      error ("Rule with name '" ^ name ^ "' does not exist")
    end


let resolve_ions single ions world =
  let lst = List.map (fun ion -> single ion world) ions in
  match List.separate is_ok lst with
  | oks, [] -> ok (List.map strip_ok oks)
  | _, errs -> error (List.map strip_error errs)


let resolve_ions_member = resolve_ions resolve_ion_member
let resolve_ions_row = resolve_ions resolve_ion_row
let resolve_ions_rule = resolve_ions resolve_ion_rule

let rec collect_errors = function
  | (Error errs) :: t -> errs :: collect_errors t
  | (Ok oks) :: t -> collect_errors t
  | [] -> []


