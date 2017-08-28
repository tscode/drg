
open Ext_std
open Cmdliner

let run port save load wname gname gpwd = 
  let world = match load, (wname, gname, gpwd) with
  | "", ("", _, _) ->
    error ("The world's name [-w, --world-name] must be " ^
    "given if no initial world file [-l, --load-path] is provided.")
  | "", (_, "", _) ->
    error ("God's name [-g, --god-name] must be " ^
    "given if no initial world file [-l, --load-path] is provided.")
  | "", (_, _, "") ->
    error ("God's password [-P, --god-pwd] must be " ^
    "given if no initial world file [-l, --load-path] is provided.")
  | "", (w, g, p) -> 
    ok (World.create ~name:w ~god:(Member.create g p) ())
  | path, _ -> 
    let world = World.load path
    in print_endline ("Loaded world file '" ^ path ^ "'."); 
    world
  in
  match world with
  | Error msg -> `Error (false, msg)
  | Ok world ->
    match Server.run ~port world |> World.save save with
    | () -> `Ok (print_endline ("Saved world file '" ^ save ^ "'."))


let port =
  let doc = "TCP/IP port that the server listens on." in
  Arg.(value & opt int 5678 & info ["p"; "port"] ~docv:"PORT" ~doc)


let save =
  let doc = "Path of the world-file used for saving." in
  Arg.(value & opt string "./world.drg" 
             & info ["s"; "save-path"] ~docv:"PATH" ~doc)

let load = 
  let doc = "Path of the world-file used for loading the 
  initial world. The options -w, -g, and -P must be provided 
  if no load path is specified." in
  Arg.(value & opt string "" 
             & info ["l"; "load-path"] ~docv:"PATH" ~doc)

let wname = 
  let doc = "Name of the world. Only used if no initial world 
  file is provided." in
  Arg.(value & opt string ""
             & info ["w"; "world-name"] ~docv:"WORLD" ~doc)

let gname =
  let doc = "Name of the world's god. Only used if no initial world 
  file is provided." in
  Arg.(value & opt string ""
             & info ["g"; "god-name"] ~docv:"GOD" ~doc)

let gpwd =
  let doc = "Password needed to act in god's name. Only used if no 
  initial world file is provided." in
  Arg.(value & opt string ""
             & info ["P"; "god-password"] ~docv:"PASSWORD" ~doc)


let cmd =
  let doc = "Democratic Role Play Game Server" in
  let man = [
    `S Manpage.s_bugs;
    `P "Report bugs via issues on ???"
  ] in
  Term.(ret (const run $ port $ save $ load $ wname $ gname $ gpwd)),
  Term.info "drgs" ~version:"v0.1" ~doc ~exits:Term.default_exits ~man

let () = Term.(exit @@ eval cmd)

