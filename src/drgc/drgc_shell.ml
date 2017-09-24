
(*
 *
 * TODO: Logout, fail messages (replace std_formatter)
 * other commands than implement
 *
 *)



open Ext_std
open Drgc_util

open Lwt
open Cmdliner


let implement_t conn_t = let open Drgc_impl in
  Term.(const body $ am $ aa $ ar $ dm $ da $ dr $ conn_t),
  Term.(info "implement")

let propose_t conn_t = let open Drgc_prop in
  Term.(const body $ am $ aa $ ar $ dm $ da $ dr $ conn_t),
  Term.(info "propose")

let vote_t conn_t = let open Drgc_vote in
  Term.(const body $ bid $ dec $ conn_t),
  Term.(info "vote")

let act_t conn_t = let open Drgc_act in
  Term.(const body $ ion $ form $ conn_t),
  Term.(info "act")

let show_t conn_t = let open Drgc_show in
  Term.(const body $ sel $ conn_t),
  Term.(info "show")


let create_connection conn =
  match%lwt conn () with
  | Error err -> return (error err)
  | Ok conn -> return (ok (fun () -> conn))


let close_connection ?(newline=false) conn =
  let h = if newline then "\n" else "" in
  match%lwt conn () with
  | Error err -> return (ok (h ^ err))
  | Ok conn -> Client.logout conn
    >> return (ok (h ^ "Exiting..."))


let parse_input input =
  let cmd = "drgc-shell" in
  match String.split_on_char ' ' input with
  | [] | [""] -> [| cmd |]
  | tokens -> Array.of_list (cmd :: tokens)


let default = 
  Term.const (return (None, cmd_err "")),
  Term.(info "Error")

let commands conn =
  let conn_t = Term.(const conn) in [
  implement_t conn_t;
  propose_t conn_t;
  vote_t conn_t;
  act_t conn_t;
  show_t conn_t
]

let eval argv conn =
  let pe = function
    | "" -> return true
    | "Closed connection." -> 
      let msg = "Server closed connection." in
      Lwt_io.eprintl msg >> return false
    | err -> Lwt_io.eprintl err >> return true
  in
  let err = Format.str_formatter in
  match Term.eval_choice ~err ~argv default (commands conn) with
  | `Ok body -> begin
    match%lwt body with
    | _, `Error (_, err) -> pe err
    | _, `Ok msg    -> Lwt_io.printl msg >> return true
    end
  | _ -> let err = Format.flush_str_formatter () in
    let err = Bytes.(sub err 0 (index err '\n')) in
    pe err
    


let rec shell_loop conn =
  Lwt_io.print "drgc-shell> " >>
  let%lwt input = Lwt_io.read_line_opt Lwt_io.stdin in
  match input with
  | None -> close_connection ~newline:true conn
  | Some input ->
    match Bytes.trim input with
    | "exit" -> close_connection conn
    | _ ->
    let argv = parse_input input in
    match%lwt eval argv conn with
    | false -> close_connection conn
    | true  -> shell_loop conn


let main conn =
  match%lwt conn () with
  | Error err -> return (error err)
  | Ok conn -> shell_loop (fun () -> return (ok conn))

let shell conn = match Lwt_main.run (main conn) with
  | Error err -> `Error (false, err)
  | Ok msg -> print_endline msg; `Ok ()

let shell_t =
  let doc = "shell for democratic rule games (drg)" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  let man = help_secs in
  Term.(ret (const shell $ Drgc_login.login_t)),
  Term.info "drgc" ~version:"v0.1" ~doc ~sdocs ~exits ~man 
 
