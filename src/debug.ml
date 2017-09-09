

open Value

let form = Rule.Form.(
  create ()
  |> write_raw "self" (Raw.string "Myum")
  |> write_raw "bonus" (Raw.int 7)
  |> write_raw "cost" (Raw.int 7)
  (*|> write_raw "money" (Raw.int 3)*)
  |> write_raw "you" (Raw.int 42)
  |> finalize
)

let prog = Rule.Program.create Rule.Program.[
  add Type.Int (mem_dynamic "self") "money" (var_typed Type.Int "bonus");
  (*add Type.Int "Myum" "money" (var Type.Int "boni");*)
  (*add Type.Int "Myum" "money" (var Type.Int "cost" - int 3);*)
  add Type.Int (mem_dynamic "self") "money" (var_listed [Raw.int 3; Raw.int 7] "cost");
  add Type.Int (mem_static "Hao") "dog" (int 2);
  tgg (mem_static "Muyum") "cursed"
]

let spec = Rule.Program.spec prog
let () = print_string "Spec created\n"


let party = Party.(
  create ()
  |> add_member ~id:0 "Hao" "hispwd"
  |> add_member ~id:1 "Myum" "herpwd"
  |> add_int_row ~id:2 "money" 0
  |> add_int_row ~id:3 "dog" 0
  |> add_bool_row ~id:4 "cursed" false
  (*|> del_member "Hao"*)
)

let spec' = Rule.Spec.diff form party spec
let () = print_string (Rule.Spec.show spec')
let () = print_string "\n"
let () = print_string (Party.show party)

let party = Rule.Program.exec form prog party
let () = print_string "\n"
let () = print_string (Party.show party)

