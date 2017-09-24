
open Ext_std
open Value
open Id
open World

open Game
open Lwt

open Drgc_util


module Atom = struct

open Fmt

(*
 * -n : maximal number of lines in list-output
 * -d : format of dates. Default: "%d.%m.%y %H:%M"
 * -D : format of dated quantities. Default: "%d.%m.%y %H:%M: %v\n"
 * -L : format of listed quantities. Default: "%v\n"
 * -f : from
 *
 * name
 *
 * god
 * god.(name|pwd)
 *
 * !god
 * !god.(name|pwd)
 *
 * rules
 * [rules.]!rules
 * [rules.]!rules.(name|descr|cat|spec|prog|targets|date)
 *
 * party
 * [party.].(members|attribs|!mem|!attr)
 * [party.][members.]!mem.(name|pwd|!attr)
 * [party.][members.]!mem.!attr.(name|def)
 * [party.][attribs.]!attr.(name|def)
 * >> [party.][members.]!mems.rules
 *
 * ballots
 * [ballots.]!ballots
 * [ballots.]!ballots.(status|yes|no|mods|mod#)
 *
 * history
 * history#
 * [history.]!events
 * [history.]!events.(type|mod#|mem|attr)
 *
 *)

(*let member attrs ion world =*)

   

end


let show_entry id entry = "  " ^ id_to_str id ^ " " ^ entry

let show_entries f clist =
  let names = Counted.List.map f clist in
  Counted.List.mapid show_entry names
  |> List.map Counted.p_dat
  |> String.concat "\n"


let show_rules rules = show_entries (Rule.p_name $ fst) rules

let show_ballots ballots world =
  let conv ballot = 
    let yes, no = Ballot.p_status ballot |> List.separate snd in
    let names arg = strip_ok (Util.Resolve.IdName.members (List.map fst arg) world) in
    "yes: " ^ Bytes.concat "," (names yes) ^ "  no: " ^ Bytes.concat "," (names no)
  in
  show_entries conv ballots


let show_party party = let open Printf in

  let show_row r = 
    let name = sprintf "%8s:" (Row.p_name r) in
    let vals = match r with
    | Row.Int    row -> List.map ((sprintf "%8d") $ List.hd) row.Row.cells
    | Row.Float  row -> List.map ((sprintf "%8f") $ List.hd) row.Row.cells
    | Row.Bool   row -> List.map ((sprintf "%8b") $ List.hd) row.Row.cells
    | Row.String row -> List.map ((sprintf "%8s") $ List.hd) row.Row.cells
    in
    Bytes.concat " " (name :: vals)
  in

  let show_header party =
    let names = List.map ((sprintf "%8s") $ Member.p_name) (Party.p_members_dat party) in
    Bytes.concat " " (sprintf "%8s" "" :: names)
  in

  let table = show_header party :: List.map show_row (Party.p_rows_dat party)
  in
  Bytes.concat "\n" table

let show_world world =
  List.iter print_endline [
    "Name: " ^ world.name;
    "God: " ^ Member.p_name world.god;
    "Party:";
    show_party (p_party world);
    "Rules:";
    show_rules (p_rules world);
    "Ballots:";
    show_ballots (p_ballots world) world
  ]



let show conn = let main =
  match%lwt conn () with
  | Error err -> Lwt_io.eprintl err
  | Ok conn ->
    match%lwt Client.(w_logout (w_world (ok $ show_world)) conn) with
    | Error err -> Lwt_io.eprintl err
    | Ok () -> return ()
  in
  cmd_ok (Lwt_main.run main)


open Cmdliner

let show_t =
  let doc = "display information about the drg world" in
  let exits = Term.default_exits in
  let man = [
    `S Manpage.s_description;
    `P "This command can be used to get information about the world. This
    is only a preliminary implementation as of yet.";
    `Blocks help_secs
  ] in
  Term.(ret (const show $ Drgc_login.login_t)),
  Term.(info "show" ~doc ~sdocs:Manpage.s_common_options ~exits ~man)

