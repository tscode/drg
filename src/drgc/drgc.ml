
open Ext_std
open Value
open Id
open World

open Game
open Lwt

(*open Drgc_act*)

let commands = [
  Drgc_vote.vote_t; 
  Drgc_prop.propose_t;
  Drgc_impl.implement_t;
  Drgc_act.act_t;
  Drgc_show_.show_t;
]

let default = 
  Drgc_shell.shell_t


open Cmdliner

let () = Term.(exit @@ eval_choice default commands)
  
