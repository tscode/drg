
open Ext_std
open World


exception Auth_error of bytes

type ion =
  | Id of int
  | Name of bytes
  [@@deriving show, yojson]

type card = {
  ion : ion;
  pwd : bytes
} [@@deriving show, yojson]


let card ion pwd = { ion; pwd }
let id i = Id i
let name n = Name n

let set_id i card = { card with ion = id i }
let set_name n card = { card with ion = name n }

let auth_id id pwd world =
  match id = -1, pwd = Member.pwd world.god with
  | true, true -> ok id
  | true, false -> 
    error ("Wrong password for god '" ^ (Member.name world.god) ^ "'")
  | false, _ ->
    let a = Party.has_member_w_id id world.party in
    let b = Party.is_authorized_w_id id pwd world.party in
    match a, b with
    | true, false -> error ("Wrong password for member " ^ Id.str id)
    | false, _    -> error ("Member " ^ Id.str id ^ " does not exist")
    | true, true -> ok id

let auth_name name pwd world =
  match (name = Member.name world.god) with
  | true  -> auth_id (-1) pwd world
  | false -> 
    match Party.id_w_name name world.party with
    | id -> auth_id id pwd world
    | exception Not_found -> 
      error ("Member '" ^ name ^ "' does not exist")


let auth {ion; pwd} world = match ion with
  | Id id -> auth_id id pwd world
  | Name name -> auth_name name pwd world

let auth_exn card world =
  match auth card world with
  | Ok id -> id
  | Error msg -> raise (Auth_error msg)


let obscure_pack pack = List.map (function
  | Mod.Add_member mem -> Mod.Add_member (Member.obscure mem)
  | a -> a
  ) pack

let obscure_event = function
  | Event.Impl l -> Event.Impl (obscure_pack l)
  | Event.Prop l -> Event.Prop (obscure_pack l)
  | a -> a

let obscure card world =
  match auth card world with
  | Error _ as err -> err
  | Ok (-1) -> ok world
  | Ok id ->
    let god = Member.obscure world.god in
    let party = Party.obscure ~except_id:id world.party in
    let ballots = 
      Id.map (fun (pack, info) -> (obscure_pack pack, info)) world.ballots
    in
    ok { world with god; party; ballots }

let obscure_exn card world = 
  match obscure card world with
  | Ok world -> world
  | Error err -> raise (Auth_error err)


let ion_of_string ion_str =
  try id (int_of_string ion_str) with
  | Failure _ -> name ion_str
