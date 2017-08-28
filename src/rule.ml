
open Ext_std

module Form = Rule_form

module Spec = Rule_spec

module Program = Rule_program


type header = {
  name  : bytes;
  descr : bytes;
  cat  : bytes;
  tags : bytes Ordered.t;
  spec : Spec.t
} [@@deriving show, yojson]

type t = {
  header : header;
  program : Program.t
} [@@deriving show, yojson]


let create ?(tags=[]) ~name ~descr ~cat program = {
  header = {
    name;
    descr;
    cat;
    tags = Ordered.create tags;
    spec = Program.spec program;
  };
  program
}

let header r = r.header
let program r = r.program

let name r = r.header.name
let has_name n r = (name r = n)
