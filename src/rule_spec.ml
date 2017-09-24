
open Ext_std
open Value

exception Inconsistent_spec of bytes

type mem =
  | Static
  | Dynamic
  [@@deriving show, yojson]

type t = {
  typed  : (bytes * Type.t) list;
  listed : (bytes * Raw.t list) list;
  rows    : (bytes * Type.t) list;
  members : (bytes * mem) list;

} [@@deriving show, yojson]


let add_typed a sp  = { sp with typed = a :: sp.typed }
let add_listed a sp = { sp with listed = a :: sp.listed }
let add_row rname t sp = { sp with rows = (rname, t) :: sp.rows }

let add_static_member mem sp = 
  { sp with members = (mem, Static) :: sp.members }

let add_dynamic_member dmem sp =
  { sp with members = (dmem, Dynamic) :: sp.members } 
  |> add_typed (dmem, Type.String)


let p_typed sp  = sp.typed
let p_listed sp = sp.listed
let p_members sp  = sp.members
let p_rows sp = sp.rows

let cmp a a' = try Util.cmp_assoc a a' with
  | Util.Inconsistent_assoc -> raise (Inconsistent_spec (fst a))


let finalize sp =
  let typed = List.sort_uniq cmp (p_typed sp) in
  let listed = (p_listed sp)
    |> List.map (fun (n, l) -> (n, List.sort_uniq Pervasives.compare l)) 
    |> List.sort_uniq cmp
  in
  let members = List.sort_uniq Pervasives.compare (p_members sp) in
  let rows = List.sort_uniq cmp (p_rows sp)
  in
  { typed; listed; members; rows }


let create ?(typed=[]) ?(listed=[]) ?(members=[]) ?(rows=[]) () = 
  finalize { typed; listed; members; rows }

let merge sp sp' =
  let sp, sp' = finalize sp, finalize sp' in {
    typed   = List.append (p_typed   sp) (p_typed   sp');
    listed  = List.append (p_listed  sp) (p_listed  sp');
    members = List.append (p_members sp) (p_members sp');
    rows    = List.append (p_rows    sp) (p_rows    sp')
  }

let collect spl = List.fold_left merge (create ()) spl

let resolve_mem form sp = List.map (function
  | (a, Static) -> (a, Static)
  | (b, Dynamic) -> 
    match Rule_form.read_raw b form with
    | Ok (Raw.String b) -> (b, Static)
    | _ -> (b, Dynamic)
  ) sp.members 
  |> List.sort_uniq Util.cmp

let cmp_mem a b = Util.cmp (fst a) b

let diff form party sp =
  let typed_check =
    List.map (fun (n,r) -> (n, Raw.to_type r)) form
    |> List.sort_uniq Util.cmp_assoc
  in
  let listed_check = form in
  let members_check = 
    Party.p_members_name party
    |> List.sort Util.cmp
  in
  let rows_check =
    List.map (fun r -> Row.(p_name r, to_type r)) Party.(p_rows_dat party)
    |> List.sort_uniq Util.cmp_assoc
  in
  let typed   = List.diff Util.cmp (p_typed sp) typed_check in
  let listed  = List.diff Util.cmp_subr (p_listed sp) listed_check in
  let members = List.diff cmp_mem (resolve_mem form sp) members_check in
  let rows    = List.diff Util.cmp (p_rows sp) rows_check 
  in
  create ~typed ~listed ~members ~rows ()

