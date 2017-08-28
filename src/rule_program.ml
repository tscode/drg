
open Ext_std
open Value

exception Execution_error of bytes
exception Inconsistent_program of bytes
exception Internal_error of bytes



module Tree = struct

type terminal = (Raw.t, bytes) result

and var = 
  | Typed  of Type.t     * bytes
  | Listed of Raw.t list * bytes
  [@@deriving show, yojson]

and attr =
  | Last of Type.t * bytes * bytes
  | Mean of Type.t * bytes * bytes * int
  [@@deriving show, yojson]
  (*| Mean_date of Type.t * bytes * bytes * float*)

and mem =
  | Static of bytes
  | Dynamic of bytes
  [@@deriving show, yojson]

and cmd =
  | Set of Type.t * mem * bytes * expr
  | Add of Type.t * mem * bytes * expr
  | Tgg of Type.t * mem * bytes
  [@@deriving show, yojson]


and unary =
  | Neg of expr
  | Inv of expr
  [@@deriving show, yojson]

and binary =
  | Plus of expr * expr
  | Mult of expr * expr
  | Ge   of expr * expr
  | Le   of expr * expr
  | Eq   of expr * expr
  [@@deriving show, yojson]

and branch =
  | Ite of expr * expr * expr
  [@@deriving show, yojson]

and expr =
  | Term   of terminal
  | Var    of var
  | Attr   of attr
  | Cmd    of cmd
  | Unary  of unary
  | Binary of binary
  | Branch of branch
  [@@deriving show, yojson]

end


module Eval = struct
open Tree

let type_str = function
  | Raw.Int    _ -> "Int" 
  | Raw.Float  _ -> "Float"
  | Raw.Bool   _ -> "Bool"
  | Raw.String _ -> "String"

let type_str2 raw raw' = 
  type_str raw ^ ", " ^ type_str raw'

let type_error raw op = 
  Error ("Wrong type '" ^ raw ^ "' in '" ^ op ^ "'")

(* Unary operations *)

let rec eval_neg form party ex = 
  match eval form party ex with 
  | Ok (Raw.Int i)   -> Ok (Raw.int (-i))
  | Ok (Raw.Float f) -> Ok (Raw.float (-.f))
  | Ok (Raw.Bool b)  -> Ok (Raw.bool (not b))
  | Error _  as err  -> err
  | Ok raw           -> type_error (type_str raw) "Neg"

and eval_inv form party ex = 
  match eval form party ex with
  | Ok (Raw.Float f) -> Ok (Raw.float (1./.f))
  | Error _  as err  -> err
  | Ok raw           -> type_error (type_str raw) "Inv"

and eval_unary form party = function
  | Neg ex -> eval_neg form party ex
  | Inv ex -> eval_inv form party ex


(* Binary operations *)

and eval_plus form party ex ex' =
  match eval form party ex, eval form party ex' with
  | Ok (Raw.Int i),   Ok (Raw.Int i')   -> Ok (Raw.int (i + i'))
  | Ok (Raw.Float f), Ok (Raw.Float f') -> Ok (Raw.float (f +. f'))
  | (Error _ as err), _ -> err
  | _, (Error _ as err) -> err
  | Ok raw, Ok raw'     -> type_error (type_str2 raw raw') "Plus"

and eval_mult form party ex ex' =
  match eval form party ex, eval form party ex' with
  | Ok (Raw.Int i),   Ok (Raw.Int i')   -> Ok (Raw.int (i * i'))
  | Ok (Raw.Float f), Ok (Raw.Float f') -> Ok (Raw.float (f *. f'))
  | (Error _ as err), _ -> err
  | _, (Error _ as err) -> err
  | Ok raw, Ok raw'     -> type_error (type_str2 raw raw') "Mult"

and eval_ge form party ex ex' =
  match eval form party ex, eval form party ex' with
  | Ok (Raw.Int i),   Ok (Raw.Int i')   -> Ok (Raw.bool (i >= i'))
  | Ok (Raw.Float f), Ok (Raw.Float f') -> Ok (Raw.bool (f >= f'))
  | (Error _ as err), _ -> err
  | _, (Error _ as err) -> err
  | Ok raw, Ok raw'     -> type_error (type_str2 raw raw') "Ge"

and eval_le form party ex ex' =
  match eval form party ex, eval form party ex' with
  | Ok (Raw.Int i),   Ok (Raw.Int i')   -> Ok (Raw.bool (i <= i'))
  | Ok (Raw.Float f), Ok (Raw.Float f') -> Ok (Raw.bool (f <= f'))
  | (Error _ as err), _ -> err
  | _, (Error _ as err) -> err
  | Ok raw, Ok raw'     -> type_error (type_str2 raw raw') "Le"

and eval_eq form party ex ex' =
  match eval form party ex, eval form party ex' with
  | Ok (Raw.Int i),    Ok (Raw.Int i')    -> Ok (Raw.bool (i = i'))
  | Ok (Raw.Float f),  Ok (Raw.Float f')  -> Ok (Raw.bool (f = f'))
  | Ok (Raw.Bool f),   Ok (Raw.Bool f')   -> Ok (Raw.bool (f = f'))
  | Ok (Raw.String f), Ok (Raw.String f') -> Ok (Raw.bool (f = f'))
  | (Error _ as err), _ -> err
  | _, (Error _ as err) -> err
  | Ok raw, Ok raw'     -> type_error (type_str2 raw raw') "Eq"

and eval_binary form party = function
  | Plus (ex, ex') -> eval_plus form party ex ex'
  | Mult (ex, ex') -> eval_mult form party ex ex'
  | Ge   (ex, ex') -> eval_ge   form party ex ex'
  | Le   (ex, ex') -> eval_le   form party ex ex'
  | Eq   (ex, ex') -> eval_eq   form party ex ex'
  

(* Ternary operations *)

and eval_branch form party (Ite (cond, ex, ex')) =
  match eval form party cond with
  | Ok (Raw.Bool b) -> if b 
      then eval form party ex 
      else eval form party ex'
  | Error _ as err -> err
  | Ok raw         -> type_error (type_str raw) "Ite"


(* Form variables *)

and eval_var form party = function
  | Typed (typ, name) -> Ok (Rule_form.read_raw_typed typ name form)
  | Listed (l, name)  -> Ok (Rule_form.read_raw_listed l name form)


(* Attribute evaluations *)

and eval_last form party m r t = begin match t with
  | Type.Int    -> Raw.int    (Party.get Row.get_int    m r !party)
  | Type.Float  -> Raw.float  (Party.get Row.get_float  m r !party)
  | Type.Bool   -> Raw.bool   (Party.get Row.get_bool   m r !party)
  | Type.String -> Raw.string (Party.get Row.get_string m r !party)
  end
  |> fun x -> Ok x

and eval_mean form party m r n = function
  | Type.Int -> Party.get Row.get_int_cell m r !party
    |> List.filter (( > ) n)
    |> List.fold_left ( + ) 0
    |> float_of_int 
    |> ( *. ) (1. /. float_of_int(n))
    |> fun x -> Ok (Raw.float x)
  | Type.Float -> Party.get Row.get_float_cell m r !party
    |> List.filter (( > ) (float_of_int n))
    |> List.fold_left ( +. ) 0.
    |> ( *. ) (1. /. float_of_int(n))
    |> fun x -> Ok (Raw.float x)
  | _ -> Error "Cannot calculate 'Mean' for non Int/Float"

and eval_attr form party = function
  | Last (t, m, r) -> eval_last form party m r t
  | Mean (t, m, r, n) -> eval_mean form party m r n t


(* Command executions *)

and resolve_mem form = function
  | Static a -> a
  | Dynamic a -> match Rule_form.read_raw_typed Type.string a form with
    | Raw.String a -> a 
    | _ -> raise (Internal_error "failed to resolve member")

and eval_set form party m r v = 
  let curr = Party.get Row.get_raw m r !party in
  match eval form party v with
  | Error _ as err -> err
  | Ok raw -> 
    let s = Ok ( Raw.string (
      "Successfully set value " ^ Raw.to_string raw ^ 
      " to attribute " ^ r ^ " of member " ^ m ) )
    in 
    let open Raw in
    match raw, curr with
    | Int    a, Int    _ -> party := Party.push_int    m r a !party; s
    | Float  a, Float  _ -> party := Party.push_float  m r a !party; s
    | Bool   a, Bool   _ -> party := Party.push_bool   m r a !party; s
    | String a, String _ -> party := Party.push_string m r a !party; s
    | raw, raw'          -> type_error (type_str2 raw raw') "Set"


and eval_add form party m r v = 
  let curr = Party.get Row.get_raw m r !party in
  match eval form party v with
  | Error _ as err -> err
  | Ok raw -> 
    let s = Ok ( Raw.string (
      "Successfully added value " ^ Raw.to_string raw ^ 
      " to attribute " ^ r ^ " of member " ^ m ) )
    in 
    let open Raw in
    match raw, curr with
    | Int    a, Int    b -> party := Party.push_int   m r (a +  b) !party; s
    | Float  a, Float  b -> party := Party.push_float m r (a +. b) !party; s
    | raw, raw'          -> type_error (type_str2 raw raw') "Add"


and eval_tgg form party m r = 
  let s = Ok ( Raw.string (
      "Successfully toggled attribute" ^ r ^ " of member " ^ m ) )
  in
  match Party.get Row.get_raw m r !party with
  | Raw.Bool a -> party := Party.push_bool   m r (not a) !party; s
  | raw        -> type_error (type_str raw) "Tgg"


and eval_cmd form party = function
  | Set (_, m, r, v) -> eval_set form party (resolve_mem form m) r v
  | Add (_, m, r, v) -> eval_add form party (resolve_mem form m) r v
  | Tgg (_, m, r)    -> eval_tgg form party (resolve_mem form m) r

and eval form party = function
  | Term   t  -> t
  | Var    v  -> eval_var form party v
  | Attr   a  -> eval_attr form party a
  | Cmd    c  -> eval_cmd form party c
  | Unary  op -> eval_unary form party op
  | Binary op -> eval_binary form party op
  | Branch br -> eval_branch form party br

end

open Tree
open Eval

(*type expr = Tree.expr*)
type t = expr list [@@deriving show, yojson]

let int a = Term (Ok (Raw.int a))
let float a = Term (Ok (Raw.float a))
let bool a = Term (Ok (Raw.bool a))
let string a = Term (Ok (Raw.string a))

let var_typed t n = Var (Typed (t, n))
let var_listed l n = Var (Listed (l, n))

let mem_static a = Static a
let mem_dynamic a = Dynamic a

let branch a b c = Branch (Ite (a,b,c))

let attr t m r = Attr (Last (t, m, r))
let mean_attr t m r n = Attr (Mean (t, m, r, n))

let add t m r e = Cmd (Add (t, m, r, e))
let set t m r e = Cmd (Add (t, m, r, e))
let tgg m r   = Cmd (Tgg (Type.Bool, m, r))

let (~-) a = Unary (Neg a)
let inv  a = Unary (Inv a)
let ( + ) a b = Binary (Plus (a, b))
let ( - ) a b = Binary (Plus (a, ~-b))
let ( * ) a b = Binary (Mult (a, b))
let ( / ) a b = Binary (Mult (a, inv b))
let ( = ) a b = Binary (Eq (a, b))
let ( <= ) a b = Binary (Le (a, b))
let ( >= ) a b = Binary (Ge (a, b))

let eval = eval

let exec form prog party = 
  let p = ref party in
  match prog 
  |> List.map (eval form p)
  |> List.find_all (function Error _ -> true | _ -> false)
  with
  | [] -> Ok !p
  | Error err :: _ -> Error err
  | _ -> Error "Unexpected error during execution"


let exec_exn form prog party = 
  match exec form prog party with
  | Ok party -> party
  | Error err -> raise (Execution_error err)



let rec spec_expr = function
  | Term _
  | Attr _ -> Rule_spec.create ()

  | Cmd (Tgg (t, Static m,r)) -> Rule_spec.create ()
    |> Rule_spec.add_static_member m
    |> Rule_spec.add_row r t

  | Cmd (Set (t, Static m,r,ex))
  | Cmd (Add (t, Static m,r,ex)) -> spec_expr ex 
    |> Rule_spec.add_static_member m 
    |> Rule_spec.add_row r t

  | Cmd (Tgg (t, Dynamic m,r)) -> Rule_spec.create ()
    |> Rule_spec.add_dynamic_member m
    |> Rule_spec.add_row r t

  | Cmd (Set (t, Dynamic m,r,ex))
  | Cmd (Add (t, Dynamic m,r,ex)) -> spec_expr ex 
    |> Rule_spec.add_dynamic_member m 
    |> Rule_spec.add_row r t

  | Unary (Neg ex)
  | Unary (Inv ex) -> spec_expr ex

  | Binary (Plus (ex, ex'))
  | Binary (Mult (ex, ex'))
  | Binary (Ge (ex, ex'))
  | Binary (Le (ex, ex'))
  | Binary (Eq (ex, ex')) -> Rule_spec.collect [
      spec_expr ex;
      spec_expr ex'       
    ]

  | Branch (Ite (ex, ex', ex'')) -> Rule_spec.collect [
      spec_expr ex;
      spec_expr ex';
      spec_expr ex''
    ]

  | Var a -> match a with
    | Typed (t,n) -> Rule_spec.(create () |> add_typed (n,t))
    | Listed (l,n) -> Rule_spec.(create () |> add_listed (n,l))


let spec prog = 
  List.map spec_expr prog 
  |> Rule_spec.collect
  |> Rule_spec.finalize


let check_consistency prog = ignore (spec prog); prog
  
let create l = check_consistency l


