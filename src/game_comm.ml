
open Ext_std
open World


module Q = struct

type t =
  | World of Game_auth.card
  | Name  of Game_auth.card
  | God   of Game_auth.card
  | Party of Game_auth.card
  | Rules of Game_auth.card
  | Ballots of Game_auth.card
  | History of Game_auth.card * int
  | Event   of Game_auth.card * Event.t 
  [@@deriving show, yojson]


let encode q = to_yojson q |> Yojson.Safe.to_string

let decode str =
  match Yojson.Safe.from_string str |> of_yojson with
  | Result.Ok a -> ok a
  | _ -> error "Could not parse query"
  | exception Yojson.Json_error _ -> error "Could not parse query"

end

module A = struct

type t =
  | World of World.t
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


let process_world world =
  (world, A.World world)

let process_name world =
  (world, A.Name world.name)

let process_god world =
  (world, A.God world.god)

let process_party world =
  (world, A.Party world.party)

let process_rules world =
  (world, A.Rules world.rules)

let process_ballots world =
  (world, A.Ballots world.ballots)

let process_history n world =
  if n <= 0 
  then (world, A.History world.past)
  else (world, A.History (List.filteri (fun i x -> i < n) world.past))

let process_event card e world =
  let world, msg_res = Game_dynamic.evolve card e world in
  (world, A.Event msg_res)
  (*match Game_dynamic.evolve card e world with*)
  (*| Ok    (world, msg) -> (world, A.Event (ok msg))*)
  (*| Error (world, msg) -> (world, A.Event (error msg))*)


let process world query = try
  let open Game_auth in match query with
  | Q.World card -> process_world (obscure_exn card world)
  | Q.Name  card -> process_name  (obscure_exn card world)
  | Q.God   card -> process_god   (obscure_exn card world)
  | Q.Party card -> process_party (obscure_exn card world)
  | Q.Rules card -> process_rules (obscure_exn card world) 
  | Q.Ballots  card     -> process_ballots (obscure_exn card world) 
  | Q.History (card, n) -> process_history n (obscure_exn card world)
  | Q.Event   (card, e) -> process_event card e world
  with
  | Game_auth.Auth_error m -> (world, A.Auth m)



