
open Ext_std
open Game
open Lwt


module Util = Client_util

type conn = {
  status : bool;
  card : Auth.card;
  addr : bytes;
  port : int;
  ic : Lwt_io.input Lwt_io.channel;
  oc : Lwt_io.output Lwt_io.channel
}

let ret_error err = return (error err)
let ret_ok a = return (ok a)

let query qu conn =
  let str = Comm.Q.encode qu in
  Lwt_io.write_line conn.oc str >> 
  match%lwt Lwt_io.read_line_opt conn.ic with
  | None -> return (error "Closed connection.")
  | Some str -> match Comm.A.decode str with
    | Error err -> ret_error ("Failure receiving answer: " ^ err ^ ".")
    | Ok ans    -> ret_ok ans
  
let login ~port ~addr card =
  let ip = Unix.inet_addr_of_string addr in
  try%lwt
  let%lwt ic, oc = Lwt_io.open_connection (Unix.ADDR_INET (ip, port)) in
  let conn = { status = false; card; addr; port; ic; oc } in
  match%lwt query (Comm.Q.Login card) conn with
  | Error err -> ret_error err
  | Ok ans -> match ans with
    | Comm.A.Login (Ok _) -> ret_ok { conn with status = true }    
    | Comm.A.Login (Error err)
    | Comm.A.Error err -> ret_error err
    | _ -> ret_error "Severe internal inconsistency."
  with
  | Unix.Unix_error(Unix.ECONNREFUSED, _, _) -> ret_error "Connection refused."
  | Unix.Unix_error(Unix.ENETUNREACH, _, _) -> ret_error "Network unreachable."

let logout conn = Lwt_io.close conn.ic >> Lwt_io.close conn.oc

(*let renew conn =*)
  (*logout conn >>= fun conn -> *)
  (*login conn.port conn.addr conn.card*)

let query_world conn = query (Comm.Q.World) conn >>= function
  | Ok (Comm.A.World world) -> ret_ok world
  | Ok _ -> ret_error "Inconsistent answer"
  | Error err -> ret_error err

let query_name conn = query (Comm.Q.Name) conn >>= function
  | Ok (Comm.A.Name name) -> ret_ok name
  | Ok _ -> ret_error "Inconsistent answer"
  | Error err -> ret_error err

let query_god conn = query (Comm.Q.God) conn >>= function
  | Ok (Comm.A.God god) -> ret_ok god
  | Ok _ -> ret_error "Inconsistent answer"
  | Error err -> ret_error err

let query_party conn = query (Comm.Q.Party) conn >>= function
  | Ok (Comm.A.Party party) -> ret_ok party
  | Ok _ -> ret_error "Inconsistent answer"
  | Error err -> ret_error err

let query_rules conn = query (Comm.Q.Rules) conn >>= function
  | Ok (Comm.A.Rules rules) -> ret_ok rules
  | Ok _ -> ret_error "Inconsistent answer"
  | Error err -> ret_error err

let query_ballots conn = query (Comm.Q.Ballots) conn >>= function
  | Ok (Comm.A.Ballots ballots) -> ret_ok ballots
  | Ok _ -> ret_error "Inconsistent answer"
  | Error err -> ret_error err

let query_history n conn = query (Comm.Q.History n) conn >>= function
  | Ok (Comm.A.History hist) -> ret_ok hist
  | Ok _ -> ret_error "Inconsistent answer"
  | Error err -> ret_error err


let w_query g f conn = match%lwt g conn with
  | Error err -> ret_error err
  | Ok a -> return (f a)

let w_world f conn = w_query query_world f conn
let w_name  f conn = w_query query_name f conn
let w_god   f conn = w_query query_god f conn
let w_party f conn = w_query query_party f conn
let w_rules f conn = w_query query_rules f conn
let w_ballots   f conn = w_query query_ballots f conn
let w_history n f conn = w_query (query_history n) f conn


let w_logout f conn =
  let%lwt res = f conn in
  logout conn >> return res

let run ~port ~addr card f =
  match%lwt login ~port ~addr card with
  | Ok conn -> w_logout f conn
  | Error msg -> return (error msg)


let propose pack conn = 
  query (Comm.Q.Event (World.Event.Prop pack)) conn >>= function
  | Ok (Comm.A.Event ans) -> ret_ok ans
  | Ok _ -> ret_error "Inconsistent answer"
  | Error err -> ret_error err

let vote id v conn =
  query (Comm.Q.Event (World.Event.Vote (id, v))) conn >>= function
  | Ok (Comm.A.Event ans) -> ret_ok ans
  | Ok _ -> ret_error "Inconsistent answer"
  | Error err -> ret_error err

let implement pack conn =
  query (Comm.Q.Event (World.Event.Prop pack)) conn >>= function
  | Ok (Comm.A.Event ans) -> ret_ok ans
  | Ok _ -> ret_error "Inconsistent answer"
  | Error err -> ret_error err

let act id form conn =
  query (Comm.Q.Event (World.Event.Act (id, form))) conn >>= function
  | Ok (Comm.A.Event ans) -> ret_ok ans
  | Ok _ -> ret_error "Inconsistent answer"
  | Error err -> ret_error err




