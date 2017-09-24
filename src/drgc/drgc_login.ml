
open Ext_std
open Value
open Id
open World

open Game
open Lwt

open Drgc_util


let card user pwd = match int_of_string user with
  | uid -> Api.card (Ion.Id uid) pwd
  | exception (Failure _) -> Api.card (Ion.Name user) pwd

let login port addr user pwd = match user, pwd with
  | None, None -> 
    cmd_err ("User [-u, --user] must be specified")

  | Some u, None -> 
    cmd_err ("Password [-P, --pwd] for user " ^ u ^ " must be given")  

  | None, Some _ -> 
    cmd_err ("Password [-P, --pwd] must not be given if no member

    [-m, --member] is provided") 
  | Some u, Some p -> cmd_ok (fun () -> Client.login ~port ~addr (card u p))


open Cmdliner

let login_t = 
  let docs = Manpage.s_common_options in
  let port =
    let doc = "TCP/IP port that the client connects to." in
    Arg.(value & opt int 5678 & info ["p"; "port"] ~docv:"PORT" ~docs ~doc)
  in
  let addr =
    let doc = "Ip address of the server." in
    Arg.(value & opt string "127.0.0.1" 
               & info ["i"; "ip"] ~docv:"IP" ~docs ~doc)
  in
  let user =
    let doc = "Id or name of the drg user. Used for login. If no value is
    provided, a guest account is used [not implemented!]." in
    Arg.(value & opt (some string) None
               & info ["u"; "user"] ~docv:"NAME/ID" ~docs ~doc)
  in
  let pwd =
    let doc = "Password of the member. Used for login. Must be given if
    [-m, --member] is provided." in
    Arg.(value & opt (some string) None
               & info ["P"; "password"] ~docv:"PASSWORD" ~docs ~doc)
  in
  Term.(ret (const login $ port $ addr $ user $ pwd))
