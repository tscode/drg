
open Ext_std
open Value
open Id
open World

open Lwt
open Cmdliner

exception Drgc_internal

let cmd_ok  a = `Ok a
let cmd_err a = `Error (false, a)

let ret_cmd_ok a = return (`Ok a)
let ret_cmd_err a = return (`Error (false, a))

let construct_body msg f conn =
  match%lwt conn with
  | Error err  -> return (None, cmd_err err)
  | Ok    conn -> 
    let co = Some conn in
    match%lwt f conn with
    | Error err -> return (co, cmd_err err)
    | Ok ans -> 
      let cc = Bytes.concat "\n" in
      match ans with
      | Error errs -> return (co, cmd_err (cc ("Error:" :: errs)))
      | Ok    msgs -> return (co, cmd_ok  (cc (msg :: msgs)))

let run_body body conn =
  let main =
    match%lwt body conn with
    | None, res -> return res
    | Some conn, res -> Client.logout conn >> return res
  in
  match Lwt_main.run main with
  | `Error err -> `Error err
  | `Ok a -> match a with
    | "" -> `Ok ()
    | a  -> print_endline a; `Ok ()


let help_secs = [
 `S Manpage.s_common_options;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
 `S Manpage.s_bugs; `P "Report bugs to github.com/tscode/drg.";]



let parse_pack cmd am aa ar dm da dr =

  let no_mods am aa ar dm da dr =
    List.for_all ((=) []) [am; aa; ar] && List.for_all ((=) []) [dm; da; dr]
  in

  let complete header fields input = 

    let read_field field = 
      print_string ("  " ^ field ^ ": "); 
      read_line ()
    in

    let rec aux fields input = match fields, input with
      | h :: t, h' :: t' -> let i = (if h' == "" then read_field h else h') in 
        i :: aux t t' 
      | h :: t, [] -> let i = read_field h in 
        i :: aux t []
      | [], _ :: _ -> raise Drgc_internal
      | [], [] -> []
    in

    (* TODO: This is buggy, right now because cmdliner neglects empty inputs *)
    let input' = List.filter ((!=) "") input in
    match List.length fields, List.length input', List.length input with
    | n, m', m when (n = m') && (n = m) -> ok input
    | n, _,  m when n < m -> 
      error ("Too many arguments for " ^ header ^ " " ^ List.hd input ^ ".")
    | _, _, _ -> 
        print_endline ("Specify " ^ header ^ " " ^ List.hd input ^ ":");
        ok (aux fields input)
  in 

  let mod_add_mem = function
    | [name; pwd] -> Mod.add_mem (Member.create name pwd)
    | _ -> raise Drgc_internal
  in

  let mod_add_attr = function
    | [name; value] -> Mod.add_row (Named.of_raw name (Raw.of_string value))
    | _ -> raise Drgc_internal
  in 

  let mod_add_rule = function
    | [name; descr; targets; cat; prog] ->
      let targets = String.split_on_char ':' targets in
      let ions = List.map Ion.of_string targets in
      begin match Rule.Prog.parse prog with
        | Error err -> error ("Failed to parse program:" ^ err)
        | Ok prog -> 
          let rule = Rule.create ~name ~descr ~cat prog in
          ok (Mod.add_rule ions rule)
      end
    | _ -> raise Drgc_internal
  in  

  match no_mods am aa ar dm da dr with
  | true -> 
    let msg = ("Must specify at least one modification for command " ^ cmd) in
    Error msg
  | false ->
    let am = List.map (complete "member" ["Name"; "Password"]) am in
    let aa = List.map (complete "attribute" ["Name"; "Initial"]) aa in
    let ar =
      let fields = ["Name"; "Description"; "Targets"; "Category"; "Program"] in
      List.map (complete "rule" fields) ar
    in
    match List.collect_results (List.concat [am; aa; ar]) with
    | Error errs -> error (List.hd errs)
    | Ok _ ->
      let dm = List.map Ion.of_string dm in
      let da = List.map Ion.of_string da in
      let dr = List.map Ion.of_string dr
      in
      match List.collect_results (List.map (mod_add_rule $ strip_ok) ar) with
      | Error errs -> error (List.hd errs)
      | Ok rules -> ok (List.concat [
        List.map (mod_add_mem  $ strip_ok) am;
        List.map (mod_add_attr $ strip_ok) aa;
        rules;
        List.map Mod.del_mem  dm;
        List.map Mod.del_row  da;
        List.map Mod.del_rule dr;
      ])


