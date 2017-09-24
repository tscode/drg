
open Value
open Angstrom


module P = struct

let concat = Bytes.concat ""

let is_space = function ' '  | '\t' -> true | _ -> false
let is_eol   = function '\r' | '\n' -> true | _ -> false
let is_digit = function '0' .. '9'  -> true | _ -> false

let is_init_tok = function
  | 'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | '_'        -> true
  | _ -> false

let is_tok tok = is_init_tok tok || is_digit tok

let single c = char c >>| Char.escaped
let minus = single '-'
let plus  = single '+'
let dot   = single '.'
let comma = single ','
let colon = single ':'
let excl  = single '!'
let semic = single ';'

let sat f = satisfy f >>| Char.escaped

let eol    = sat is_eol
let eols   = take_while1 is_eol
let space  = take_while1 is_space
let space' = take_while is_space

let space_eols = space <|> space' *> eols *> space'

let surrounded ?(strict=false) l r p = match strict with
  | true  -> char l *> p <* char r
  | false -> char l *> space' *> p <* space' <* char r

let parens   p = surrounded '(' ')' p
let brackets p = surrounded '[' ']' p
let quoted   p = surrounded ~strict:true '"' '"' p

let token = lift2 (^) (sat is_init_tok) (option "" (take_while1 is_tok))
let uint  = take_while1 is_digit

let pair ?(strict=true) sep a b =
  let tup = lift2 (fun x y -> (x,y)) in
  match strict with
  | true  -> tup (a <* sep) b
  | false -> tup (a <* space' <* sep) (space' *> b)

let triple ?(strict=true) sep1 sep2 a b c =
  pair ~strict sep1 a (pair ~strict sep2 b c) >>|
  fun (a, (b, c)) -> (a, b, c)

let int =
  let pos = option "" plus *> uint in
  let neg = lift2 (^) minus uint in
  pos <|> neg


let float =
  list [ int; dot; option "" uint ] >>| concat

let bool = string "true" <|> string "false"

let string = char '"' *> take_till ((=) '"') <* char '"'

let list p = brackets (sep_by (space' *> comma <* space') p)

let terminal = bool <|> string <|> float <|> int

end


let typ = 
  let int    = string "int"    *> return Type.int    in
  let float  = string "float"  *> return Type.float  in
  let bool   = string "bool"   *> return Type.bool   in
  let string = string "string" *> return Type.string
  in
  int <|> float <|> bool <|> string

let raw = P.terminal >>| Raw.of_string

let terminal = raw >>| Rule_prog.terminal

let var = 
  let typed = 
    P.pair P.colon P.token typ >>| 
    fun (n, t) -> Rule_prog.var_typed t n 
  in
  let listed = 
    P.pair P.colon P.token (P.list raw) >>| 
    fun (n, l) -> Rule_prog.var_listed l n 
  in
  typed <|> listed


let attr' =
  let dyn = (P.excl *> return false) <|> return true in
  let fetch d (m, r, t) = match d with
  | true  -> t, Rule_prog.mem_dynamic m, r
  | false -> t, Rule_prog.mem_static  m, r
  in
  lift2 fetch dyn P.(triple dot colon token token typ)

let attr = attr' >>| fun (t, m, r) -> Rule_prog.attr t m r

let unary  n op ex = lift  op (string n *> P.space *> ex)
let binary n op ex = lift2 op (string n *> P.space *> ex) (P.space *> ex)

let ops expr = List.map ((|>) expr) [
  unary  "neg"       Rule_prog.neg;
  unary  "inv"       Rule_prog.inv;
  binary "or"        Rule_prog.bool_or;
  binary "and"       Rule_prog.bool_and;
  binary "plus"      Rule_prog.plus;
  binary "minus"     Rule_prog.minus;
  binary "mult"      Rule_prog.mult;
  binary "div"       Rule_prog.div;
  binary "equal"     Rule_prog.equal;
  binary "less"      Rule_prog.less;
  binary "lesseq"    Rule_prog.lesseq;
  binary "greater"   Rule_prog.greater;
  binary "greatereq" Rule_prog.greatereq;
]


let branch_expr expr = lift3 Rule_prog.branch 
  (string "if" *> P.space *> expr <* P.space)
  (string "then" *> P.space *> expr <* P.space)
  (string "else" *> P.space *> expr)

let expr_builder expr = choice (
  terminal :: var :: attr :: P.parens expr :: branch_expr expr :: ops expr
)

let expr = fix expr_builder


let branch_cmd cmd = lift3 Rule_prog.branch
  (string "if" *> P.space *> expr <* P.space)
  (string "then" *> P.space *> cmd)
  (option (Rule_prog.string "") (P.space *> string "else" *> P.space *> cmd))

let set = lift2 (fun (t, m, r) e -> Rule_prog.set t m r e)
  (string "set" *> P.space *> attr')
  (P.space *> expr)

let add = lift2 (fun (t, m, r) e -> Rule_prog.add t m r e)
  (string "add" *> P.space *> attr')
  (P.space *> expr)

let toggle = lift (fun (t, m, r) -> Rule_prog.toggle t m r)
  (string "toggle" *> P.space *> attr')

let cmd_builder cmd = set <|> add <|> toggle <|> branch_cmd cmd

let cmd = fix cmd_builder


let prog =
  let ws = skip_many (P.space <|> P.eol <|> P.semic) <|> return () in
  let sep = (P.space' *> (P.eol <|> P.semic)) *> ws in
  ws *> sep_by sep cmd <* ws <* end_of_input

let parse str = parse_only prog (`String str)
