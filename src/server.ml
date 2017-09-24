
open Ext_std
open Value
open Id
open Game
open Lwt


(* Auxilliary Definitions *)

let answer_login qu w = snd (Api.process 0 qu !w)
let ret_error msg = return (error msg)
let ret_ok id = return (ok id)
let send_answer oc ans = Lwt_io.write_line oc (Api.A.encode ans)


(* Login Query *)

let handle_login query worldref = match query with
  | Api.Q.Login card -> 
    begin match answer_login query worldref with
    | Api.A.Login (Ok id) -> ret_ok id
    | Api.A.Login (Error msg) -> ret_error msg
    | _ -> ret_error "Internal inconsistency."
    end
  | _ -> ret_error "Queries can only be handled after logging in."

let login worldref ic oc () =
  Lwt_io.read_line_opt ic >>= function 
  | None -> ret_error "Connection closed before login."
  | Some str -> match Api.Q.decode str with
    | Ok query -> handle_login query worldref
    | Error e  -> ret_error e


(* Non-login Query *)

let handle_query id query worldref =
  let world, ans = Api.process id query !worldref in
  worldref := world; 
  Lwt_io.print "Query handled: " 
  >> Lwt_io.printl (Api.Q.headstring query)
  >> return ans

let string_of_addr = function
  | Unix.ADDR_UNIX file -> "unix:" ^ file
  | Unix.ADDR_INET (ip, port) -> 
      Unix.string_of_inet_addr ip ^ ":" ^ string_of_int(port)

let close_channels ic oc =
  Lwt_io.close ic >> Lwt_io.close oc >> return ()

let rec connection_loop id worldref ic oc () =
  Lwt_io.read_line_opt ic >>= function
  | None -> return () 
  | Some str -> begin 
    match Api.Q.decode str with
    | Error e -> return (Api.A.Error ("Failure decoding query: " ^ e))
    | Ok query -> handle_query id query worldref
    end
    >>= send_answer oc
    >>= connection_loop id worldref ic oc


(* Handle single connection *)

let rec handle_connection worldref ip (ic, oc) =
  Lwt_io.print ("Player [" ^ string_of_addr ip ^ "] connected... ")
  >>= login worldref ic oc >>= begin function
    | Ok id ->
      Lwt_io.printl ("Login as member " ^ Ion.str (Ion.id id) ^ " successful.")
      >>  send_answer oc (Api.A.Login (Ok id))
      >>= connection_loop id worldref ic oc 
    | Error msg -> 
      Lwt_io.printl ("Login failed: " ^ msg ^ ".")
      >> send_answer oc (Api.A.Login (error msg))
      >> Lwt_unix.sleep 0.001
    end
    >>  Lwt_io.printl ("Player [" ^ string_of_addr ip ^ "] disconnected.")
    >>  close_channels ic oc

let register_signal_handlers quit =
  let f s _ = 
    print_endline ("Received signal " ^ s); 
    ignore (quit ())
  in
  Lwt_unix.on_signal Sys.sigterm (f "TERM") |> ignore;
  Lwt_unix.on_signal Sys.sigint  (f "INT") |> ignore

let terminator () =
  let t, u = Lwt.wait () in
  let f () = Lwt.wakeup u () in
  t, f

let run ?(port=5678) world =
  let addr = "127.0.0.1" in
  let worldref = ref world in
  let wait_quit, quit = terminator () in
  let handler = handle_connection worldref in
  let () = register_signal_handlers quit in
  let ip = 
    Lwt_unix.ADDR_INET (Unix.inet_addr_of_string addr, port) 
  in
  let main () = try%lwt
    let%lwt server = 
      Lwt_io.establish_server_with_client_address ip handler
    in
    Lwt_io.printl ("Server listens to " ^ string_of_addr ip ^ ".")
    >> wait_quit 
    >> Lwt_io.printl ("Shutting down server...")
    >> Lwt_io.shutdown_server server
    >> Lwt_io.printl ("Server shutted down successfully.")
    >> return (ok !worldref)
    with
    | Unix.Unix_error(Unix.EADDRINUSE, _, _) -> 
      return (error "Address already in use.")
  in
  Lwt_main.run (main ())

