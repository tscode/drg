
open Ext_std
open Game
open Lwt


let handle_query query worldref =
  let world, ans = Comm.process !worldref query in
  worldref := world; 
  Lwt_io.printl "Query handled" >> return ans

let send_answer oc ans =
  Lwt_io.write_line oc (Comm.A.encode ans)

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

let rec connection_loop worldref ic oc () =
  Lwt_io.read_line_opt ic >>= function
  | None -> return () 
  | Some str -> begin 
    match Comm.Q.decode str with
    | Error e -> return (Comm.A.Error ("Failure parsing message: " ^ e))
    | Ok query -> handle_query query worldref
    end
    >>= send_answer oc
    >>= connection_loop worldref ic oc

let rec handle_connection worldref ip (ic, oc) =
  Lwt_io.printl ("Player connected from " ^ string_of_addr ip)
  >>= connection_loop worldref ic oc 
  >>  Lwt_io.printl ("Player from " ^ string_of_addr ip ^ " disconnected")
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

