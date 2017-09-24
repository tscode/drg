

open Ext_std
open Value
open Id

module Mod    = World_mod
module Event  = World_event
module Ballot = World_ballot


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
  rules   : Rule.book;
  past    : (Event.t    * event_info) Counted.clist;
  ballots : Ballot.box;
  count   : Counter.t
} [@@deriving show, yojson]


let create ~name ~god () = {
  name;
  god;
  party   = Party.create ();
  rules   = [];
  past    = [];
  ballots = [];
  count   = Counter.create ()
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

let p_name w = w.name
let p_god w = w.god
let p_party w = w.party
let p_rules w = w.rules
let p_past w = w.past
let p_ballots w = w.ballots
