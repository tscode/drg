
open Ext_std
open Value
open Id
open World

open Game
open Lwt

open Drgc_util

let body am aa ar dm da dr conn =
  match parse_pack "implement" am aa ar dm da dr with
  | Error err -> return (None, cmd_err err)
  | Ok pack ->
    let msg = "Implementation was successful" in
    construct_body msg (Client.implement pack) (conn ())
  
let implement am aa ar dm da dr conn = 
  run_body (body am aa ar dm da dr) conn


open Cmdliner

let am =
  let doc = "Name and password of a member to be added 
  to the system.  Leaving out the password will cause 
  drgc to ask for it explicitly." in
  Arg.(value & opt_all (list string) [] 
             & info ["m"; "add-member"] ~docv:"NAME,PWD" ~doc)

let aa =
  let doc = "Name and default value of an attribute to 
  be implemented.  Leaving out the default value will 
  cause drgc to ask for it explicitly." in
  Arg.(value & opt_all (list string) [] 
             & info ["a"; "add-attrib"] ~docv:"NAME,VAL" ~doc)

let ar =
  let doc = "Name, description, targets, category, 
  and program of a rule to be implemented. Leaving out 
  some of these entries will cause drgc to ask for them 
  explicitly.  Target members are separated by ':', and 
  can be specified via id or name." in
  let docv = "NAME,DESCR,T1:...:TN,CAT,PROG" in
  Arg.(value & opt_all (list string) [] 
             & info ["r"; "add-rule"] ~docv ~doc)

let dm =
  let doc = "Name or Id of a member that is to be deleted" in
  Arg.(value & opt_all string [] 
             & info ["M"; "del-member"] ~docv:"ID/Name" ~doc)

let da =
  let doc = "Name or Id of an attribute to be deleted" in
  Arg.(value & opt_all string [] 
             & info ["A"; "del-attr"] ~docv:"ID/Name" ~doc)

let dr =
  let doc = "Name or Id of a rule to be deleted" in
  Arg.(value & opt_all string [] 
             & info ["R"; "del-rule"] ~docv:"ID/Name" ~doc)




let implement_t =
  let doc = "implement drg modifications" in
  let exits = Term.default_exits in
  let man = [
    `S Manpage.s_description;
    `P "With this command, modifications can be implemented 
    the drg system without confirmation by voting. This only
    works when logged in as god.";
    `Blocks help_secs
  ] in
  Term.(ret (const implement $ am $ aa $ ar $ dm $ da $ dr $ Drgc_login.login_t)),
  Term.(info "implement" ~doc ~sdocs:Manpage.s_common_options ~exits ~man)

