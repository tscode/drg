
open Ext_std
open Game
open Lwt

let port = 5678
let addr = "127.0.0.1"
let card = Auth.card (Auth.name "Budda") "supersecret"

(*let login = Comm.Q.to_yojson (Comm.Q.Login card) |>  Yojson.Safe.to_string*)
(*let () = print_endline login*)


let party = Lwt_main.run @@ Client.run ~port ~addr card Client.party 

let () = match party with
| Ok party -> Party.show party |> print_endline
| Error msg -> print_endline msg
