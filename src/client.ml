
open Ext_std
open Game
open Lwt


type conn = {
  status : bool;
  card : Api.card;
  addr : bytes;
  port : int;
  ic : Lwt_io.input Lwt_io.channel;
  oc : Lwt_io.output Lwt_io.channel
}

let ret_error err = return (error err)
let ret_ok a = return (ok a)

let query qu conn =
  let str = Api.Q.encode qu in
  Lwt_io.write_line conn.oc str >> 
  match%lwt Lwt_io.read_line_opt conn.ic with
  | None -> ret_error "Closed connection."
  | Some str -> match Api.A.decode str with
    | Error err -> ret_error ("Failure receiving answer: " ^ err ^ ".")
    | Ok ans    -> ret_ok ans
  
let login ~port ~addr card =
  let ip = Unix.inet_addr_of_string addr in
  try%lwt
  let%lwt ic, oc = Lwt_io.open_connection (Unix.ADDR_INET (ip, port)) in
  let conn = { status = false; card; addr; port; ic; oc } in
  match%lwt query (Api.Q.Login card) conn with
  | Error err -> ret_error err
  | Ok ans -> match ans with
    | Api.A.Login (Ok _) -> ret_ok { conn with status = true }    
    | Api.A.Login (Error err)
    | Api.A.Error err -> ret_error err
    | _ -> ret_error "Severe internal inconsistency."
  with
  | Unix.Unix_error(Unix.ECONNREFUSED, _, _) -> ret_error "Connection refused."
  | Unix.Unix_error(Unix.ENETUNREACH, _, _) -> ret_error "Network unreachable."

let logout conn = Lwt_io.close conn.ic >> Lwt_io.close conn.oc

let query_world conn = query (Api.Q.World) conn >>= function
  | Ok (Api.A.World world) -> ret_ok world
  | Ok (Api.A.Error error) -> ret_error error
  | Ok _ -> ret_error "Inconsistent answer"
  | Error err -> ret_error err

let query_history n conn = query (Api.Q.History n) conn >>= function
  | Ok (Api.A.History hist) -> ret_ok hist
  | Ok (Api.A.Error  error) -> ret_error error
  | Ok _ -> ret_error "Inconsistent answer"
  | Error err -> ret_error err


let w_query g f conn = match%lwt g conn with
  | Error err -> ret_error err
  | Ok a -> return (f a)

let w_world f conn = w_query query_world f conn
let w_history n f conn = w_query (query_history n) f conn

let w_logout f conn =
  let%lwt res = f conn in
  logout conn >> return res

let run ~port ~addr card f =
  match%lwt login ~port ~addr card with
  | Ok conn -> w_logout f conn
  | Error msg -> return (error msg)


let propose pack conn = 
  query (Api.Q.Event (World.Event.Prop pack)) conn >>= function
  | Ok (Api.A.Event ans) -> ret_ok ans
  | Ok _ -> ret_error "Inconsistent answer"
  | Error err -> ret_error err

let vote id v conn =
  query (Api.Q.Event (World.Event.Vote (id, v))) conn >>= function
  | Ok (Api.A.Event ans) -> ret_ok ans
  | Ok _ -> ret_error "Inconsistent answer"
  | Error err -> ret_error err

let implement pack conn =
  query (Api.Q.Event (World.Event.Impl pack)) conn >>= function
  | Ok (Api.A.Event ans) -> ret_ok ans
  | Ok _ -> ret_error "Inconsistent answer"
  | Error err -> ret_error err

let act ion form conn =
  query (Api.Q.Event (World.Event.Act (ion, form))) conn >>= function
  | Ok (Api.A.Event ans) -> ret_ok ans
  | Ok _ -> ret_error "Inconsistent answer"
  | Error err -> ret_error err

