

open Ext_std
open Value
open Id
open World

open Game_util


type card = {
  ion : Ion.t;
  pwd : bytes
} [@@deriving show, yojson]

let card ion pwd = { ion; pwd }



module Q = struct

type t =
  | World
  (*| Name*)
  (*| God*)
  (*| Party*)
  (*| Rules*)
  (*| Ballots*)
  | History of int
  | Event of Event.t
  | Login of card
  [@@deriving show, yojson]

(*type t =*)
  (*| World*)
  (*| Name*)
  (*| God*)
  (*| Party*)
  (*| Rules*)
  (*| Ballots*)
  (*| History of int*)
  (*| Event of Event.t*)
  (*| Login of card*)
  (*[@@deriving show, yojson]*)

let encode qu = Yojson.Safe.to_string (to_yojson qu)

let decode str =
  match of_yojson (Yojson.Safe.from_string str) with
  | Result.Ok a -> ok a
  | _ -> error "Could parse but not understand query"
  | exception Yojson.Json_error _ -> error "Could not parse query"

let headstring = function
  | World     -> "World"
  | History n -> "History " ^ string_of_int n
  | Event _   -> "Event"
  | Login _   -> "Login"

end


module A = struct

type t =
  | World of World.t
  | History of (Event.t * World.event_info) Counted.clist
  | Event of (bytes list, bytes list) res
  | Login of (id, bytes) res
  | Error of bytes
  [@@deriving show, yojson]


let encode ans = to_yojson ans |> Yojson.Safe.to_string
let decode str =
  match Yojson.Safe.from_string str |> of_yojson with
  | Result.Ok a -> ok a
  | _ -> error "Could parse but not understand answer"
  | exception Yojson.Json_error _ -> error "Could not parse answer"

end


let process_world id world =
  let w = Censor.world id world in
  (world, A.World { w with past = [] })
  (*(world, A.World { (Censor.world id world) with past = [] })*)

let process_history id n world =
  (world, A.History (Censor.history id n world))

let process_event id ev world =
  let world, msg_res = Game_evol.evolve id ev world in
  (world, A.Event msg_res)

let process_login card world =
  match Auth.authorize card.ion card.pwd world with
  | Error msg -> (world, A.Login (error (List.hd msg)))
  | Ok id -> (world, A.Login (ok id))

let process id query world = match query with
  | Q.World      -> process_world id world
  | Q.History n  -> process_history id n world
  | Q.Event   ev -> process_event id ev world
  | Q.Login card -> process_login card world

