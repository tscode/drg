
open Ext_std
open Value

exception Inconsistent_spec of bytes

type mem =
  | Static
  | Dynamic
  [@@deriving show, yojson]



type t = {

  (* specs for form *)
  typed  : (bytes * Type.t) list;
  listed : (bytes * Raw.t list) list;

  (* specs for party *)
  rows    : (bytes * Type.t) list;
  members : (bytes * mem) list;

} [@@deriving show, yojson]

let add_typed a sp  = { sp with typed = a :: sp.typed }
let add_listed a sp = { sp with listed = a :: sp.listed }
let add_row rname t sp = { sp with rows = (rname, t) :: sp.rows }
let add_static_member mem sp = { sp with members = (mem, Static) :: sp.members }
let add_dynamic_member dmem sp =
  let sp = { sp with members = (dmem, Dynamic) :: sp.members } in
  add_typed (dmem, Type.String) sp

let typed sp  = sp.typed
let listed sp = sp.listed
let members sp  = sp.members
let rows sp = sp.rows

let cmp a a' = try Util.cmp_assoc a a' with
  | Util.Inconsistent_assoc -> raise (Inconsistent_spec (fst a))


let finalize sp =
  let typed = List.sort_uniq cmp sp.typed in
  let listed = sp.listed
  |> List.map (fun (n, l) -> (n, List.sort_uniq Pervasives.compare l)) 
  |> List.sort_uniq cmp
  in
  let members = List.sort_uniq Pervasives.compare sp.members in
  let rows = List.sort_uniq cmp sp.rows
  in
  { typed; listed; members; rows }

(*let resolve_members form sp =*)
  (*List.map (function*)
  (*| Static a -> a*)
  (*| Dynamic a -> match Rule_form.read_raw_opt a form with*)
    (*| Some (Raw.String a) -> a*)
    (*| _ -> "?" ^ a)*)
  (*(members sp)*)

let create ?(typed=[]) ?(listed=[]) ?(members=[]) ?(rows=[]) () = 
  finalize { typed; listed; members; rows }

let merge sp sp' =
  let sp, sp' = finalize sp, finalize sp' in {
    typed   = List.append sp.typed sp'.typed;
    listed  = List.append sp.listed sp'.listed;
    members = List.append sp.members sp'.members;
    rows    = List.append sp.rows sp'.rows
  }

let collect spl = List.fold_left merge (create ()) spl

(*let cmp_mem form a b = *)
let resolve_mem form sp = List.map (function
  | (a, Static) -> (a, Static)
  | (b, Dynamic) -> match Rule_form.read_raw_opt b form with
    | Some (Raw.String b) -> (b, Static)
    | _ -> (b, Dynamic)
  ) sp.members |> List.sort_uniq Util.cmp

let cmp_mem a b = Util.cmp (fst a) b

let diff form party sp =
  let typed_check =
    List.map (fun (n,r) -> (n, Raw.to_type r)) form
    |> List.sort_uniq Util.cmp_assoc
  in
  let listed_check = form in
  let members_check = 
    List.map Member.name Party.(names party) 
    |> List.sort Util.cmp
  in
  let rows_check =
    List.map (fun r -> Row.(name r, to_type r)) Party.(rows party)
    |> List.sort_uniq Util.cmp_assoc
  in
  let typed = List.diff Util.cmp (typed sp) typed_check in
  let listed = List.diff Util.cmp_subr (listed sp) listed_check in
  let members = List.diff cmp_mem (resolve_mem form sp) members_check in
  let rows = List.diff Util.cmp (rows sp) rows_check 
  in
  create ~typed ~listed ~members ~rows ()

