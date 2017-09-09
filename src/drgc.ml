
open Ext_std
open Value
open Game
open Lwt

module Mod = World.Mod


exception Drgc_internal

let card user pwd = match int_of_string user with
  | uid -> Auth.(card (Id uid) pwd)
  | exception (Failure _) -> Auth.(card (Name user) pwd)


let cmd_ok  a = `Ok a
let cmd_err a = `Error (false, a)

let login port addr user pwd = match user, pwd with
  | None, None -> cmd_ok (Client.login ~port ~addr (card "guest" ""))
  | Some u, Some p -> cmd_ok (Client.login ~port ~addr (card u p))
  | Some u, None -> 
     cmd_err ("Password [-P, --pwd] for user " ^ u ^ " must be given")  
  | None, Some _ ->
     cmd_err ("Password [-P, --pwd] must not be given if no member
     [-m, --member] is provided") 


(* VOTE command *)

let vote id dec conn = match id, dec with
  | None, _ -> cmd_err "Ballot id must be provided for command vote"
  | _, None -> cmd_err "Decision true/false must be provided for command vote"
  | Some id, Some dec -> 
    let main =
    match %lwt conn with
    | Error err -> Lwt_io.eprintl err
    | Ok conn ->
      match%lwt Client.(w_logout (vote id dec) conn) with
      | Error err -> Lwt_io.eprintl err
      | Ok ans -> match ans with
        | Error errs -> Lwt_list.iter_s Lwt_io.eprintl errs
        | Ok msgs -> Lwt_list.iter_s Lwt_io.printl msgs 
          >> Lwt_io.printl "Vote was sucessful"
  in
  cmd_ok (Lwt_main.run main)


(* PROPOSE and IMPLEMENT commands *)

let read_field field = print_string (field ^ ": "); read_line ()

let complete header fields input = 
  let rec aux fields input = match fields, input with
  | h :: t, h' :: t' -> 
    (if h' == "" then read_field h else h') :: aux t t' 
  | h :: t, [] -> read_field h :: aux t []
  | [], _ :: _ -> raise Drgc_internal
  | [], [] -> []
  in
  let fields' = List.filter ((!=) "") fields in
  match List.length fields, List.length fields', List.length input with
  | n, n', m when (n = n') && (n = m) -> ok input
  | n, _,  m when n < m -> error ()
  | _, _, _ -> 
    print_endline ("Complete " ^ header ^ " '" ^ List.hd input ^ "':");
    ok (aux fields input)


let mod_add_member = function
  | [name, pwd] -> Mod.add_member (Member.create name pwd)
  | _ -> raise Drgc_internal

let mod_add_attr = function
  | [name, value] -> Mod.add_row (Named.of_raw name (Raw.parse value))
  | _ -> raise Drgc_internal


let mod_add_rule = function
  | [name, descr, targets, cat, prog] ->
    (*TODO real PROGRAM!*)
    let ions = List.map Auth.ion_of_string targets in
    match resolve_ions_member targets with
    | Error -> 
    let targets = resolve targets in
    let prog = Rule.Program.(create [mem_dynamic "self"]) in
    let rule = Rule.create ~name ~descr ~cat prog in
    Mod.add_rule targets rule
  | _ -> raise Drgc_internal





let propose am aa ar dm da dr conn =
  match List.for_all ((=) []) [am; aa; ar; dm; da; dr] with
  | true -> cmd_err "Must specify at least one modification for command propose"
  | false -> 
    let main = 
    match%lwt conn with
    | Error err -> Lwt_io.eprintl err
    | Ok conn -> Client.w_world begin fun world ->
      let am = List.map (complete "Member" ["Name", "Password"]) am in
      let aa = List.map (complete "Attribute" ["Name", "Initial"]) aa in
      let ar =
        let fields = ["Name", "Description", "Targets", "Category", "Program"] in
        List.map (complete "Rule" fields) ar
      in
      let to_ions l = List.map Auth.ion_of_string l in
      let dm = Client_util.resolve_ions_member (to_ions dm) world in
      let da = Client_util.resolve_ions_row    (to_ions da) world in
      let dr = Client_util.resolve_ions_rule   (to_ions dr) world 
      in
      match Client_util.collect_errors [dm; da; dr] with
      | _ :: _ as l -> Lwt_list.iter_s Lwt_io.eprintl l
      | [] -> 
        let dm = strip_ok dm in
        let da = strip_ok da in
        let dr = strip_ok dr 
        in
        let pack = List.concat [
          List.map mod_add_member am;
          List.map mod_add_attr   aa;
          List.map mod_add_rule   ar;
          List.map Mod.del_member dm;
          List.map Mod.del_row    da;
          List.map Mod.del_rule   dr;
        ] 
        in
        match%lwt Client.(w_logout (propose pack) conn) with
        | Error err -> Lwt_io.eprintl err
        | Ok ans -> match ans with
          | Error errs -> Lwt_list.iter_s Lwt_io.eprintl errs
          | Ok msgs -> Lwt_list.iter_s Lwt_io.printl msgs
            >> Lwt_io.printl "Proposal was sucessful"
      end
      in
      cmd_ok (Lwt_main.run main)



open Cmdliner

let help_secs = [
 `S Manpage.s_common_options;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";`Noblank;
 `P "Use `$(mname) help patterns' for help on patch matching."; `Noblank;
 `P "Use `$(mname) help environment' for help on environment variables.";
 `S Manpage.s_bugs; `P "Check bug reports at http://bugs.example.org.";]


let login_t = 
  let docs = Manpage.s_common_options in
  let port =
    let doc = "TCP/IP port that the client connects to." in
    Arg.(value & opt int 5678 & info ["p"; "port"] ~docv:"PORT" ~docs ~doc)
  in
  let addr =
    let doc = "Ip address of the server." in
    Arg.(value & opt string "127.0.0.1" 
               & info ["a"; "address"] ~docv:"IP" ~docs ~doc)
  in
  let user =
    let doc = "Id or name of the member used for login. If no value is
    provided, a guest account is used." in
    Arg.(value & opt (some string) None
               & info ["m"; "member"] ~docv:"NAME/ID" ~docs ~doc)
  in
  let pwd =
    let doc = "Password of the member used for login. Must be given if
    [-m, --member] is provided." in
    Arg.(value & opt (some string) None
               & info ["P"; "password"] ~docv:"PASSWORD" ~docs ~doc)
  in
  Term.(ret (const login $ port $ addr $ user $ pwd))

let vote_t =
  let bid =
    let doc = "Id of the proposal for which the vote should be casted" in
    Arg.(value & pos 0 (some int) None & info [] ~docv:"ID" ~doc)
  in
  let dec =
    let doc = "Decision (yes/no) for the vote." in
    Arg.(value & pos 1 (some bool) None & info [] ~docv:"DECISION" ~doc)
  in
  let doc = "Vote for or against a proposal." in
  let exits = Term.default_exits in
  let man = [
    `S Manpage.s_description;
    `P "With this command, the currently logged in member can cast his/her
    vote for/against an existing proposal.";
    `Blocks help_secs
  ] in
  Term.(ret (const vote $ bid $ dec $ login_t)),
  Term.(info "vote" ~doc ~sdocs:Manpage.s_common_options ~exits ~man)


let default_t =
  let doc = "cli client for democratic rule games" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  let man = help_secs in
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ login_t)),
  Term.info "drgc" ~version:"v0.1" ~doc ~sdocs ~exits ~man 
  
  
  let () = Term.(exit @@ eval_choice default_t [vote_t])
  
  
