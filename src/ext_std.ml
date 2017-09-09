
exception Not_implemented

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b
  [@@deriving show, yojson]


let ok a = Ok a
let error a = Error a

let is_ok = function Ok _ -> true | _ -> false
let is_error a = not (is_ok a)

let strip_ok = function Ok a -> a | _ -> raise Not_found
let strip_error = function Error a -> a | _ -> raise Not_found

let identity a = a

module Util = struct

exception Inconsistent_assoc

let cmp = Pervasives.compare

let cmp_assoc (a,b) (a',b') = 
  if a = a' then 
  begin
    if b <> b' 
    then raise Inconsistent_assoc
    else 0
  end
  else Pervasives.compare a a'

let cmp_subl (a, b) (a', l) =
  if a = a' && List.mem b l
  then 0
  else Pervasives.compare a a'

let cmp_subr (a, l) (a', b) =
  if a = a' then
  begin
    if List.mem b l
    then 0
    else 1
  end
  else Pervasives.compare a a'

end


module List = struct

include List

let rec rem f = function
  | [] -> []
  | h :: t -> match f h with
    | true -> rem f t
    | false -> h :: rem f t

let remi f l = 
  let rec aux f i = function
  | [] -> []
  | h :: t -> match f i h with
    | true -> aux f (i+1) t
    | false -> h :: aux f (i+1) t
  in
  aux f 0 l

let find_idx f l = 
  let rec aux f i = function
  | [] -> -1
  | h :: t -> match f h with
    | true -> i
    | false -> aux f (i+1) t
  in
  aux f 0 l


let rec rep f g = function
  | [] -> []
  | h :: t -> match f h with
    | true -> g h :: rep f g t
    | false -> h :: rep f g t

let repi f g l =
  let rec aux f g i = function
    | [] -> []
    | h :: t -> match f i h with
      | true -> g i h :: aux f g (i+1) t
      | false -> h :: aux f g (i+1) t
  in
  aux f g 0 l

let rec ntimes n a =
  if n <= 0 then [] else a :: ntimes (n-1) a


let rec diff cmp l l' =
  match l, l' with
  | [], _ -> []
  | _, [] -> l
  | h :: t, h' :: t' -> match cmp h h' with
    | 0 -> diff cmp t t'
    | 1 -> diff cmp l t'
    | _ -> h :: diff cmp t l'


let rec filteri f l =
  let rec aux f i = function
    | [] -> []
    | h :: t -> match f i h with
      | true  -> h :: aux f (i+1) t
      | false -> aux f (i+1) t
  in
  aux f 0 l


let separate f l = 
  let rec aux f yes no = function
    | [] -> yes, no
    | h :: t -> if f h then aux f (h :: yes) no t else aux f yes (h :: no) t
  in
  aux f [] [] l

end

module Ordered = struct

type 'a t = 'a list [@@deriving show, yojson]

let create ?(cmp=Pervasives.compare) l = List.sort_uniq cmp l

let rec insert ?(cmp=Pervasives.compare) a = function
  | [] -> [a]
  | h :: t as l -> match cmp a h with
    | -1 -> a :: l
    | 1  -> h :: insert a t
    | _  -> l

let rec remove ?(cmp=Pervasives.compare) a = function
  | [] -> []
  | h :: t as l -> match cmp a h with
    | 1 -> h :: remove a t
    | -1 -> l
    | _ -> t

end
