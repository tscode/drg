
open Ext_std
open Value
open Id
open World

open Game
open Lwt

open Drgc_util


let body id dec conn =
  match id, dec with
  | None, _ -> 
    let msg = "Ballot id must be provided for command vote" in
    return (None, cmd_err msg)
  | _, None -> 
    let msg = "Decision true/false must be provided for command vote" in 
    return (None, cmd_err msg)
  | Some id, Some dec -> 
    let msg = "Vote was successful" in
    construct_body msg (Client.vote id dec) (conn ())


let vote id dec conn = run_body (body id dec) conn


open Cmdliner


let bid =
  let doc = "Id of the proposal for which the vote should be casted" in
  Arg.(value & pos 0 (some int) None & info [] ~docv:"ID" ~doc)

let dec =
  let doc = "Decision (yes/no) for the vote." in
  Arg.(value & pos 1 (some bool) None & info [] ~docv:"DECISION" ~doc)


let vote_t =
  let doc = "vote for or against drg proposals" in
  let exits = Term.default_exits in
  let man = [
    `S Manpage.s_description;
    `P "With this command, the user can cast his/her
    vote for/against an existing proposal.";
    `Blocks help_secs
  ] in
  Term.(ret (const vote $ bid $ dec $ Drgc_login.login_t)),
  Term.(info "vote" ~doc ~sdocs:Manpage.s_common_options ~exits ~man)

