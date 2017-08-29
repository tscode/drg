

open Value
open Ext_std

module Mod = World_mod
module Event = World_event


type rule_info = {
  mems : Id.id list;
  date : Date.t
} [@@deriving show, yojson]

type event_info = {
  aid   : Id.id;
  aname : bytes;
  fail  : bool;
  date  : Date.t;
  msg   : bytes list;
} [@@deriving show, yojson]

type vote_info = 
  (int * bool) list [@@deriving show, yojson]

type t = {
  name    : bytes;
  god     : Member.t;
  party   : Party.t;
  rules   : (Rule.t     * rule_info)  Id.w_id list;
  past    : (Event.t    * event_info) Id.w_id list;
  ballots : (Mod.t list * vote_info)  Id.w_id list;
  count   : Id.counter
} [@@deriving show, yojson]


let create ~name ~god () = {
  name;
  god;
  party   = Party.create ();
  rules   = [];
  past    = [];
  ballots = [];
  count   = Id.counter ()
}



let save fname world =
  to_yojson world |> Yojson.Safe.to_file fname

let load fname =
  match Yojson.Safe.from_file fname |> of_yojson with
  | Ok game -> ok game
  | Error _ -> 
    error ("Could not load world file '" ^ fname ^ "'")
  | exception Yojson.Json_error _ -> 
    error ("Could not parse world file '" ^ fname ^ "'")


