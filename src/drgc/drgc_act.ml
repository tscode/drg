
open Ext_std
open Value
open Id
open World

open Game
open Lwt

open Drgc_util


let body ionstr formv conn =
  match ionstr with
  | "" -> 
    let msg = "Name or id of a rule must be specified for command act" in
    return (None, cmd_err msg)
  | _  ->
    let ion   = Ion.of_string ionstr in
    let nvals = List.map (fun (n,v) -> Named.of_raw n (Raw.of_string v)) formv in
    let form  = Rule.Form.(List.fold_right write_named nvals (create ())) in
    let msg   = "Action was successful" in
    construct_body msg (Client.act ion form) (conn ())


let act ionstr formv conn = run_body (body ionstr formv) conn

  
open Cmdliner

let ion =
  let doc = "Name or id of the rule to be applied." in
  Arg.(value & pos 0 string "" & info [] ~docv:"NAME/ID" ~doc)

let form =
  let doc = "Form that is used to evaluate the rule. The form is
  specified by a list of key:value pairs." in
  Arg.(value & pos 1 (list (pair ~sep:':' string string)) [] 
             & info [] ~docv:"K1:V1,...,KN:VN" ~doc)

let act_t =
  let doc = "act on attributes by applying drg rules" in
  let exits = Term.default_exits in
  let man = [
    `S Manpage.s_description;
    `P "With this command, the drg user can apply registered rules. The effect
    of applying a rule usually depends on forms. Forms are pairs of keys
    and values, which the program of the rule can access during evaluation.";
    `Blocks help_secs
  ] in
  Term.(ret (const act $ ion $ form $ Drgc_login.login_t)),
  Term.(info "act" ~doc ~sdocs:Manpage.s_common_options ~exits ~man)

