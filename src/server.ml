
open Ext_std
open Game
open Lwt

(* TODO:
    * logging
*)

(* Login Query *)

let guest_card        = Auth.card (Auth.name "Guest") ""
let ret_error msg     = return (error (Comm.A.Error msg))
let ret_card id card  = return (ok (id, Auth.set_id id card))
let login_answer q wr = snd (Comm.process guest_card q !wr)

let handle_login query worldref = match query with
  | Comm.Q.Login card -> begin 
    match login_answer query worldref with
    | Comm.A.Login (Ok id)     -> ret_card id card
    | Comm.A.Login (Error msg) -> ret_error msg
    | _ -> ret_error "Internal inconsistency"
    end
  | _ -> ret_error "Queries can only be handled after logging in."

let login worldref ic oc () =
  Lwt_io.read_line_opt ic >>= function 
  | None -> ret_error "Connection closed before login"
  | Some str -> match Comm.Q.decode str with
    | Ok query -> handle_login query worldref
    | Error er -> ret_error ("Failure during login: " ^ er)


 (* Non-login Query *)

let send_answer oc ans = Lwt_io.write_line oc (Comm.A.encode ans)

let handle_query card query worldref =
  let world, ans = Comm.process card query !worldref in
  worldref := world; Lwt_io.printl "Query handled" >> return ans

let string_of_addr = function
  | Unix.ADDR_UNIX file -> "unix:" ^ file
  | Unix.ADDR_INET (ip, port) -> 
      Unix.string_of_inet_addr ip ^ ":" ^ string_of_int(port)

let close_channels ic oc =
  Lwt_io.close ic >> Lwt_io.close oc >> return ()

let create_socket ?(backlog=10) ~port ~addr () =
  let ip = Unix.inet_addr_of_string addr in
  let sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  Lwt_unix.(bind sock (ADDR_INET (ip, port))) |> ignore;
  Lwt_unix.(listen sock backlog) |> ignore;
  sock

let rec connection_loop card worldref ic oc () =
  Lwt_io.read_line_opt ic >>= function
  | None -> return () 
  | Some str -> begin 
    match Comm.Q.decode str with
    | Error e -> return (Comm.A.Error ("Failure parsing query: " ^ e))
    | Ok query -> handle_query card query worldref
    end
    >>= send_answer oc
    >>= connection_loop card worldref ic oc

let rec handle_connection worldref ip (ic, oc) =
  Lwt_io.printl ("Player connected from " ^ string_of_addr ip ^ ".")
  >>= login worldref ic oc >>= begin function
    | Ok (id, card) ->
      Lwt_io.printl ("Login with id " ^ Id.str id ^ " successful.")
      >>= connection_loop card worldref ic oc 
    | Error ans -> 
      Lwt_io.printl ("Login failed.")
      >> send_answer oc ans
      >> Lwt_unix.sleep 0.001
    end
    >>  Lwt_io.printl ("Player from " ^ string_of_addr ip ^ " disconnected.")
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
  let main () = begin
    let%lwt server = 
      Lwt_io.establish_server_with_client_address ip handler
    in
    Lwt_io.printl ("Server listens to " ^ string_of_addr ip ^ ".")
    >> wait_quit 
    >> Lwt_io.printl ("Shutting down server...")
    >> Lwt_io.shutdown_server server
    >> Lwt_io.printl ("Server shutted down successfully.")
    >> return !worldref
    end
  in
  Lwt_main.run (main ())

