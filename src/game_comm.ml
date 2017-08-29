
open Ext_std
open World

module Auth = Game_auth


module Q = struct

type t =
  | World
  | Name
  | God
  | Party
  | Rules
  | Ballots
  | History of int
  | Event of Event.t
  | Login of Game_auth.card
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
  | World   of World.t
  | Name    of bytes
  | God     of Member.t
  | Party   of Party.t
  | Rules   of (Rule.t     * World.rule_info)  Id.w_id list
  | Ballots of (Mod.t list * World.vote_info)  Id.w_id list
  | History of (Event.t    * World.event_info) Id.w_id list
  | Event   of (bytes list, bytes list) result
  | Login   of (Id.id, bytes) result
  | Error   of bytes
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

let process_login card world =
  let ans = Auth.auth card world in 
  (world, A.Login ans)

let process card query world = try
  match query with
  | Q.World -> process_world (Auth.obscure_exn card world)
  | Q.Name  -> process_name  (Auth.obscure_exn card world)
  | Q.God   -> process_god   (Auth.obscure_exn card world)
  | Q.Party -> process_party (Auth.obscure_exn card world)
  | Q.Rules -> process_rules (Auth.obscure_exn card world) 
  | Q.Ballots    -> process_ballots (Auth.obscure_exn card world) 
  | Q.History n  -> process_history n (Auth.obscure_exn card world)
  | Q.Event   ev -> process_event card ev world
  | Q.Login card -> process_login card world
  with
  | Game_auth.Auth_error m -> (world, A.Login (error m))



