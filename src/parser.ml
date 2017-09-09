
(*module Parse = struct*)

(*open Angstrom*)

(*let uint_s = take_while1 (function '0' .. '9' -> true | _ -> false)*)
(*let uint_n = uint_s >>| int_of_string*)

(*let int_n =*)
  (*let pos = (char '+' <|> return '+') *> uint_n in*)
  (*let neg = char '-' *> uint_n >>| (fun x -> -x) in*)
  (*pos <|> neg*)

(*let float_n =*)
  (*let pre   = int_n <|> return 0 >>| float_of_int in*)
  (*let post  = uint_s >>| fun x -> "0." ^ x |> float_of_string in*)
  (*let echar = (char 'e' <|> char 'E') in*)
  (*let mag   = echar *> int_n >>| fun x -> (10.**(float_of_int x)) in*)
  (*choice [*)
    (*(pre >>= fun x -> mag >>| fun z -> x *. z);*)
    (*(pre >>= fun x -> (char '.') *> (post <|> return 0.)*)
         (*>>= fun y -> mag <|> return 1.*)
         (*>>| fun z -> (x +. if x > 0. then y else -.y) *. z)*)
  (*]*)

(*let bool_n = string "false" <|> string "true" >>| bool_of_string*)

(*let string_n = *)
  (*char '"' *> take_till (function '"' -> true | _ -> false) <* char '"'*)

(*let id_s = take_while1 (function 'a'..'z'|'A'..'Z' -> true | _ -> false)*)

(*[>let st = char '\t' <|> char ' '<]*)
(*[>let ws = skip_many1 st<]*)
(*[>let ows = skip_many st<]*)

(*[>let type_r = string ""<]*)

(*[>let formvar_decl = function<]*)

(*end*)

(*let f = match Angstrom.(parse_only Parse.string_n (`String "\" haha,\"")) with*)
  (*| Ok a -> a*)
  (*| Error err -> print_string err; "nope..."*)

(*let () = print_string f*)


