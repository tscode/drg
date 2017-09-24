
open Ext_std
open Id
open Value

module Form = Rule_form

module Spec = struct
include Rule_spec
let of_program = Rule_prog.spec
end

module Prog = struct
include Rule_prog
module Lang = Rule_lang
let parse = Lang.parse
end


type header = {
  name  : bytes;
  descr : bytes;
  cat  : bytes;
  tags : bytes Ordered.t;
  spec : Spec.t
} [@@deriving show, yojson]


type t = {
  header : header;
  program : Prog.t
} [@@deriving show, yojson]


let create ?(tags=[]) ~name ~descr ~cat program = {
  header = {
    name;
    descr;
    cat;
    tags = Ordered.create tags;
    spec = Prog.spec program;
  };
  program
}

let p_header r = r.header
let p_program r = r.program

let p_name r = r.header.name
let has_name n r = (p_name r = n)

let p_descr r = r.header.descr
let p_cat r = r.header.cat
let p_spec r = r.header.spec


type info = {
  ids  : Id.id list;
  date : Date.t
} [@@deriving show, yojson]


let info ids = { ids; date = Date.now () }

let p_ids info = info.ids
let p_date info = info.date


type book = (t * info) Counted.clist [@@deriving show, yojson]

let has_rule_w_name name book =
  match Counted.List.find ((has_name name) $ fst) book with
  | exception Not_found -> false
  | _ -> true

let has_rule_w_id id book =
  List.exists (Counted.has_id id) book

let has_rule_w_ion ion book = match ion with
  | Ion.Id id -> has_rule_w_id id book
  | Ion.Name name -> has_rule_w_name name book


let entry_w_name name book = 
  Counted.List.find ((has_name name) $ fst) book
  |> Counted.p_dat

let entry_w_id id book =
  Counted.List.findid (fun i _ -> i = id) book
  |> Counted.p_dat

let entry_w_ion ion book = match ion with
  | Ion.Id id -> entry_w_id id book
  | Ion.Name name -> entry_w_name name book


let add_rule ?(date=Date.now ()) rid ids rule book =
  match has_rule_w_name (p_name rule) book with
  | true -> raise Exists
  | false -> Counted.create rid (rule, info ids) :: book

let id_w_rule_name name book =
  Counted.(p_id (List.find ((has_name name) $ fst) book))


let del_rule n book = match n < 0 with
  | true -> raise (Invalid_argument "del_rule")
  | false -> Counted.List.remi (fun j _ -> (j = n)) book

let del_rule_w_name rname book =
  let n = Counted.List.find_index ((has_name rname) $ fst) book in
  del_rule n book

let del_rule_w_id id book =
  let n = List.find_index (Counted.has_id id) book in
  del_rule n book

let del_rule_w_ion ion book = match ion with
  | Ion.Id id -> del_rule_w_id id book
  | Ion.Name name -> del_rule_w_name name book


let entry_w_id id book =
  List.find (Counted.has_id id) book |> Counted.p_dat


