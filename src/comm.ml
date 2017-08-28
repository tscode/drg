
open Ext_std
open World

module Q = struct

type t =
  | Name  of Id.id * bytes
  | God   of Id.id * bytes
  | Party of Id.id * bytes
  | Rules of Id.id * bytes
  | Ballots of Id.id * bytes
  | History of Id.id * bytes * int
  | Event   of Id.id * bytes * Event.t
  [@@deriving show, yojson]

(*let decode str =*)

let encode q = to_yojson q |> Yojson.Safe.to_string
let decode str =
  match Yojson.Safe.from_string str |> of_yojson with
  | Result.Ok a -> ok a
  | _ -> error "Could not parse query"
  | exception Yojson.Json_error _ -> error "Could not parse query"

end

module A = struct

type t =
  | Name  of bytes
  | God   of Member.t
  | Party of Party.t
  | Rules   of (Rule.t     * World.rule_info)  Id.w_id list
  | Ballots of (Mod.t list * World.vote_info)  Id.w_id list
  | History of (Event.t    * World.event_info) Id.w_id list
  | Event   of (bytes list, bytes list) result
  | Auth of bytes
  | Error of bytes
  [@@deriving show, yojson]

let encode q = to_yojson q |> Yojson.Safe.to_string
let decode str =
  match Yojson.Safe.from_string str |> of_yojson with
  | Result.Ok a -> ok a
  | _ -> error "Could not parse answer"
  | exception Yojson.Json_error _ -> error "Could not parse answer"

end


let process_name aid world =
  (world, A.Name world.name)

let process_god aid world =
  let god =
    if aid = -1
    then world.god
    else Member.censor world.god
  in
  (world, A.God god)

let process_party aid world =
  let party = 
    if aid = -1 
    then world.party 
    else Party.censor ~except_id:aid world.party
  in
  (world, A.Party party)

let process_rules aid world =
  (world, A.Rules world.rules)

let process_ballots aid world =
  let ballots =
    if aid = -1
    then world.ballots
    else world.ballots  (* TODO: censor *)
  in
  (world, A.Ballots ballots)

let process_history aid n world =
  let history =
    if aid = -1
    then world.past
    else world.past (* TODO: censor *)
  in
  if n <= 0 
  then (world, A.History history)
  else (world, A.History (List.filteri (fun i x -> i < n) history))

let process_event aid pwd ev world =
  match Evolve.evolve aid pwd ev world with
  | Ok    (world, msg) -> (world, A.Event (ok msg))
  | Error (world, msg) -> (world, A.Event (error msg))



exception Auth_error of bytes

let verify aid pwd world =
  match aid with
  | -1 -> if pwd <> Member.pwd world.god
    then raise (Auth_error ("Wrong password for !" ^ (Member.name world.god)))
    else -1
  | _  ->
    let a = Party.has_member_w_id aid world.party in
    let b = Party.is_authorized_w_id aid pwd world.party in
    match a, b with
    | true, true -> aid
    | true, false -> 
      raise (Auth_error ("Wrong password for member " ^ Id.str aid))
    | false, _ -> 
      raise (Auth_error ("Member " ^ Id.str aid ^ " does not exist"))


let process world query = try
  match query with
  | Q.Name  (id, pwd) -> process_name  (verify id pwd world) world 
  | Q.God   (id, pwd) -> process_god   (verify id pwd world) world
  | Q.Party (id, pwd) -> process_party (verify id pwd world) world
  | Q.Rules (id, pwd) -> process_rules (verify id pwd world) world
  | Q.Ballots (id, pwd)     -> process_ballots (verify id pwd world) world
  | Q.History (id, pwd, n)  -> process_history (verify id pwd world) n world
  | Q.Event   (id, pwd, ev) -> process_event (verify id pwd world) pwd ev world
  with
  | Auth_error m -> (world, A.Auth m)



