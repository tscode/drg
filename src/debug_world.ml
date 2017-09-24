
open Value
module M = Member
module P = Party
module N = Named
module W = World
module E = World.Event
module A = World.Mod
open Evolve

let gid = 0
let gname = "Gody"
let gpwd = "godpwd"
let god = M.create gname gpwd

let rule = Rule.create ~name:"pay-food" ~descr:"hahah..." ~cat:"food"
  (Rule.Program.create Rule.Program.[
  add Type.Int (mem_dynamic "self") "money" (int 7)
])

let form = Rule.Form.(
  create ()
  |> write_raw "self" (Raw.string "M")
)

let evolve = evolve_exn gid gpwd

let world = W.create ~name:"TestWorld" ~god ()
  |> evolve (E.impl [A.add_member (M.create "T" "my")])
  |> evolve (E.impl [A.add_member (M.create "M" "her")])
  |> evolve (E.prop [A.add_row (N.int "money" 0)])
  |> evolve_exn 0 "my" (E.vote 5 true)
  |> evolve_exn 2 "her" (E.vote 5 true)
  |> evolve (E.impl [A.add_member (M.create "C" "...")])
  |> evolve_exn 0 "my" (E.prop [A.add_rule [0; 2] rule])
  |> evolve_exn 0 "my" (E.vote 13 true)
  (*|> evolve_exn 0 "my" (E.vote 13)*)
  |> evolve_exn 2 "her" (E.vote 13 true)
  |> evolve_exn 10 "..." (E.vote 13 true)
  |> evolve_exn 0 "my" (E.act 17 form)


let () = print_endline (W.show world)

module C = Comm
(*let () = print_endline (P.show world.W.party)*)
(*let world = match handle_event gid gpwd event world with*)
  (*| ok (w, msg) -> (print_endline "everything okay:";*)
  (*print_endline (show w))*)
  (*| error (w, msg) -> list.iter print_endline msg*)
