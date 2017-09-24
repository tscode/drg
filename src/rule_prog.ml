
open Ext_std
open Value


module Ast = struct

type terminal = (Raw.t, bytes) res
  [@@deriving show, yojson]

type mem =
  | Static of bytes
  | Dynamic of bytes
  [@@deriving show, yojson]

type var = 
  | Typed  of Type.t     * bytes
  | Listed of Raw.t list * bytes
  [@@deriving show, yojson]

type attr = Type.t * mem * bytes
  [@@deriving show, yojson]

type update = attr * expr
  [@@deriving show, yojson]

and op =
  | Neg of expr
  | Inv of expr
  | Add   of expr * expr
  | Sub   of expr * expr
  | Mult  of expr * expr
  | Div   of expr * expr
  | Or    of expr * expr
  | And   of expr * expr
  | Equal of expr * expr
  | Less  of expr * expr
  | Greater of expr * expr
  | Branch  of expr * expr * expr
  [@@deriving show, yojson]

and expr =
  | Term   of terminal
  | Var    of var
  | Attr   of attr
  | Op     of op
  | Update of update
  [@@deriving show, yojson]

end

module Eval = struct

open Ast
open Printf

(* error messeges *)

let type_str = Type.to_string $ Raw.to_type

let err_unary op = function
  | Error err -> error err
  | Ok r -> error 
    (sprintf "Wrong type %s in unary operation %s" 
    (type_str r) op)

let err_binary op res res' =
  match res, res' with
  | Error _, _  -> res
  | _, Error _  -> res'
  | Ok r, Ok r' -> error 
    (sprintf "Wrong types %s and %s in binary operation %s"
    (type_str r) (type_str r) op)

let err_tertiary op res1 res2 res3 =
  match res1, res2, res3 with
  | Error _, _, _ -> res1
  | _, Error _, _ -> res2
  | _, _, Error _ -> res3
  | Ok r1, Ok r2, Ok r3 -> error
    (sprintf "Wrong types %s, %s, and %s in binary operation %s"
    (type_str r1) (type_str r2) (type_str r3) op)

let err_branch = function
  | Error err -> error err
  | Ok r -> error 
    (sprintf "Wrong type %s in branching condition" (type_str r))

let err_dynamic = function
  | Ok _      -> error ("Internal error. This should not happen")
  | Error err -> error (sprintf "Failed to resolve dynamic member: %s" err)


(* unary *)

let rec eval_neg form party ex = 
  match eval form party ex with
  | Ok (Raw.Int i)   -> Ok (Raw.int (-i))
  | Ok (Raw.Float f) -> Ok (Raw.float (-.f))
  | Ok (Raw.Bool b)  -> Ok (Raw.bool (not b))
  | res -> err_unary "negate" res

and eval_inv form party ex = 
  match eval form party ex with
  | Ok (Raw.Float f) -> Ok (Raw.float (1./.f))
  | res -> err_unary "invert" res


(* binary *)

and eval_add form party ex ex' =
  match eval form party ex, eval form party ex' with
  | Ok (Raw.Int   a), Ok (Raw.Int   b) -> Ok (Raw.int   (a +  b))
  | Ok (Raw.Float a), Ok (Raw.Float b) -> Ok (Raw.float (a +. b))
  | res, res' -> err_binary "add" res res'

and eval_sub form party ex ex' =
  match eval form party ex, eval form party ex' with
  | Ok (Raw.Int   a), Ok (Raw.Int   b) -> Ok (Raw.int   (a -  b))
  | Ok (Raw.Float a), Ok (Raw.Float b) -> Ok (Raw.float (a -. b))
  | res, res' -> err_binary "add" res res'

and eval_mult form party ex ex' =
  match eval form party ex, eval form party ex' with
  | Ok (Raw.Int   a), Ok (Raw.Int   b) -> Ok (Raw.int   (a *  b))
  | Ok (Raw.Float a), Ok (Raw.Float b) -> Ok (Raw.float (a *. b))
  | res, res' -> err_binary "multiply" res res'

and eval_div form party ex ex' =
  match eval form party ex, eval form party ex' with
  | Ok (Raw.Int   a), Ok (Raw.Int   b) -> Ok (Raw.int   (a /  b))
  | Ok (Raw.Float a), Ok (Raw.Float b) -> Ok (Raw.float (a /. b))
  | res, res' -> err_binary "multiply" res res'

and eval_or form party ex ex' =
  match eval form party ex, eval form party ex' with
  | Ok (Raw.Bool b), Ok (Raw.Bool b') -> Ok (Raw.bool (b || b'))
  | res, res' -> err_binary "or" res res'

and eval_and form party ex ex' =
  match eval form party ex, eval form party ex' with
  | Ok (Raw.Bool b), Ok (Raw.Bool b') -> Ok (Raw.bool (b && b'))
  | res, res' -> err_binary "or" res res'

and eval_equal form party ex ex' =
  match eval form party ex, eval form party ex' with
  | Ok (Raw.Int    a), Ok (Raw.Int    b) -> Ok (Raw.bool (a = b))
  | Ok (Raw.Float  a), Ok (Raw.Float  b) -> Ok (Raw.bool (a = b))
  | Ok (Raw.Bool   a), Ok (Raw.Bool   b) -> Ok (Raw.bool (a = b))
  | Ok (Raw.String a), Ok (Raw.String b) -> Ok (Raw.bool (a = b))
  | res, res' -> err_binary "equal" res res'

and eval_less form party ex ex' =
  match eval form party ex, eval form party ex' with
  | Ok (Raw.Int   a), Ok (Raw.Int   b) -> Ok (Raw.bool (a < b))
  | Ok (Raw.Float a), Ok (Raw.Float b) -> Ok (Raw.bool (a < b))
  | res, res' -> err_binary "less" res res'

and eval_greater form party ex ex' =
  match eval form party ex, eval form party ex' with
  | Ok (Raw.Int   a), Ok (Raw.Int   b) -> Ok (Raw.bool (a > b))
  | Ok (Raw.Float a), Ok (Raw.Float b) -> Ok (Raw.bool (a > b))
  | res, res' -> err_binary "less" res res'

and eval_branch form party cond ex ex' =
  match eval form party cond with
  | Ok (Raw.Bool b) -> begin match b with
    | true  -> eval form party ex 
    | false -> eval form party ex'
    end
  | res -> err_branch res


and eval_op form party = function
  | Neg ex -> eval_neg form party ex
  | Inv ex -> eval_inv form party ex
  | Add   (ex, ex') -> eval_add   form party ex ex'
  | Sub   (ex, ex') -> eval_sub   form party ex ex'
  | Mult  (ex, ex') -> eval_mult  form party ex ex'
  | Div   (ex, ex') -> eval_div   form party ex ex'
  | Or    (ex, ex') -> eval_or    form party ex ex'
  | And   (ex, ex') -> eval_and   form party ex ex'
  | Equal (ex, ex') -> eval_equal form party ex ex'
  | Less  (ex, ex') -> eval_less  form party ex ex'
  | Greater (ex, ex')    -> eval_greater form party ex ex'
  | Branch  (c, ex, ex') -> eval_branch  form party c ex ex'
  
(* form variables *)

and eval_var form party = function
  | Typed (typ, name) -> Rule_form.read_raw_typed typ name form
  | Listed (l, name)  -> Rule_form.read_raw_listed l name form


(* members *)

and eval_mem form party = function
  | Static  a -> ok (Raw.string a)
  | Dynamic a -> match Rule_form.read_raw_typed Type.string a form with
    | Ok str -> ok str 
    | res -> err_dynamic res

(* attributes *)

and eval_attr form party t mem r = 
  match eval_mem form party mem with
  | Ok (Raw.String m) -> begin 
    match Party.has_member_w_name m !party with
    | false -> error (sprintf "Member %s does not exist" m)
    | true -> try ok begin match t with
      | Type.Int    -> Raw.int    (Party.get_w_name Row.get_int    m r !party)
      | Type.Float  -> Raw.float  (Party.get_w_name Row.get_float  m r !party)
      | Type.Bool   -> Raw.bool   (Party.get_w_name Row.get_bool   m r !party)
      | Type.String -> Raw.string (Party.get_w_name Row.get_string m r !party)
      end with
      | Not_found   -> error (sprintf "Row %s does not exist" r)
      | Value_error -> error 
        (sprintf "Row %s does not have type %s" r (Type.to_string t))
    end
  | Error err -> error err
  | Ok _ -> error ("Internal inconsistency. This should not happen...")


(* Command executions *)

and eval_update form party t mem r expr =
  match eval_mem form party mem with
  | Ok (Raw.String m) -> begin
    match Party.has_member_w_name m !party with
    | false -> error (sprintf "Member %s does not exist" m)
    | true  -> try match eval form party expr with
      | Error err -> error err
      | Ok raw -> party := Party.push_raw m r raw !party; 
        let raw = Raw.to_string raw in
        let msg = sprintf "Updated row %s of member %s to %s" r m raw in
        ok (Raw.string msg)
      with
      | Not_found   -> error (sprintf "Row %s does not exist" r)
      | Value_error -> error 
        (sprintf "Row %s does not have type %s" r (Type.to_string t))
    end
  | Error err -> error err
  | Ok _ -> error ("Internal inconsistency. This should not happen...")



and eval form party = function
  | Term t                -> t
  | Var v                 -> eval_var form party v
  | Op op                 -> eval_op form party op
  | Attr (t, m, r)        -> eval_attr form party t m r
  | Update ((t, m, r), e) -> eval_update form party t m r e


end

open Ast


type t = expr list [@@deriving show, yojson]


let int    a = Term (ok (Raw.int    a))
let float  a = Term (ok (Raw.float  a))
let bool   a = Term (ok (Raw.bool   a))
let string a = Term (ok (Raw.string a))

let terminal a = Term (ok a)

let var_typed  t n = Var (Typed  (t, n))
let var_listed l n = Var (Listed (l, n))

let mem_static  a = Static  a
let mem_dynamic a = Dynamic a

let branch a b c = Op (Branch (a, b, c))
let attr t m r = Attr (t, m, r)

let neg a = Op (Neg a)
let inv a = Op (Inv a)
let plus  a b = Op (Add (a, b))
let minus a b = Op (Sub (a, b))
let mult  a b = Op (Mult (a, b))
let div   a b = Op (Div (a, b))

let bool_or  a b = Op (Or (a, b))
let bool_and a b = Op (And (a, b))

let equal  a b = Op (Equal (a, b))
let less   a b = Op (Less (a, b))
let lesseq a b = bool_or (less a b) (equal a b)
let greater   a b = Op (Greater (a, b))
let greatereq a b = bool_or (greater a b) (equal a b)


let set t m r e  = Update ((t, m, r), e)
let add t m r e  = Update ((t, m, r), (plus (attr t m r) e))
let toggle t m r = Update ((t, m, r), (neg  (attr t m r)))


let exec form prog party = 
  let partyref = ref party in
  match prog 
  |> List.map (Eval.eval form partyref)
  |> List.collect_results
  with
  | Ok _ -> Ok !partyref
  | Error errs -> error errs


let rec spec_expr = function
  | Term _ -> Rule_spec.create ()
  | Attr (t, Static m, r) -> 
    Rule_spec.create ()
    |> Rule_spec.add_static_member m 
    |> Rule_spec.add_row r t
  | Attr (t, Dynamic m, r) ->
    Rule_spec.create ()
    |> Rule_spec.add_dynamic_member m 
    |> Rule_spec.add_row r t
  | Update ((t, Static m, r), ex) -> 
    spec_expr ex 
    |> Rule_spec.add_static_member m 
    |> Rule_spec.add_row r t
  | Update ((t, Dynamic m, r), ex) ->
    spec_expr ex 
    |> Rule_spec.add_dynamic_member m
    |> Rule_spec.add_row r t
  | Op (Neg ex) | Op (Inv ex) -> spec_expr ex
  | Op (Add   (ex, ex')) | Op (Sub  (ex, ex'))
  | Op (Mult  (ex, ex')) | Op (Div  (ex, ex'))
  | Op (Or    (ex, ex')) | Op (And  (ex, ex'))
  | Op (Equal (ex, ex')) | Op (Less (ex, ex'))
  | Op (Greater (ex, ex')) -> Rule_spec.collect [
      spec_expr ex;
      spec_expr ex'       
    ]
  | Op (Branch (c, ex, ex')) ->
      Rule_spec.collect [
      spec_expr c;
      spec_expr ex;
      spec_expr ex'
    ]
  | Var a -> match a with
    | Typed  (t, n) -> Rule_spec.(create () |> add_typed (n,t))
    | Listed (l, n) -> Rule_spec.(create () |> add_listed (n,l))


let rec show_expr = let open Printf in function
  | Term (Ok raw) -> Raw.to_string raw
  | Term (Error raw) -> "?"
  | Var (Typed (t, name)) -> sprintf "%s:%s" name (Type.to_string t)
  | Var (Listed (l, name)) -> sprintf "%s:%s" name (
      "[" ^ Bytes.concat ", " (List.map Raw.to_string l) ^ "]"
    )
  | Attr (t, Static  m, r) -> sprintf "!%s.%s:%s" m r (Type.to_string t)
  | Attr (t, Dynamic m, r) -> sprintf "%s.%s:%s"  m r (Type.to_string t)
  | Update (attr, ex) -> 
    sprintf "set %s %s" (show_expr (Attr attr)) (show_expr ex)
  | Op op ->
    let unary  n ex     = sprintf "(%s %s)" n (show_expr ex) in
    let binary n ex ex' = sprintf "(%s %s %s)" (show_expr ex) n (show_expr ex')
    in 
    match op with
    | Neg ex -> unary "not" ex
    | Inv ex -> unary "inv" ex
    | Add (ex, ex') -> binary "+" ex ex'
    | Sub (ex, ex') -> binary "-" ex ex'
    | Mult (ex, ex') -> binary "*" ex ex'
    | Div  (ex, ex') -> binary "/" ex ex'
    | Or  (ex, ex') -> binary "or" ex ex'
    | And  (ex, ex') -> binary "and" ex ex'
    | Equal (ex, ex') -> binary "=" ex ex'
    | Less  (ex, ex') -> binary "<" ex ex'
    | Greater (ex, ex') -> binary ">" ex ex'
    | Branch (c, (Update _ as u), (Update _ as u')) -> 
      sprintf "if %s then %s else %s" (show_expr c) (show_expr u) (show_expr u')
    | Branch (c, (Update _ as u), Term _) ->
      sprintf "if %s then %s" (show_expr c) (show_expr u)
    | Branch (c, ex, ex') ->
      sprintf "(if %s then %s else %s)" (show_expr c) (show_expr ex) (show_expr ex')


let spec prog = 
  List.map spec_expr prog 
  |> Rule_spec.collect
  |> Rule_spec.finalize

let show prog =
  List.map show_expr prog
  |> Bytes.concat "\n"

let check prog = ignore (spec prog); prog
let create l = check l
