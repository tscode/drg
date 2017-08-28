

type t = float [@@deriving show, yojson]

type 'a w_date = 'a * t [@@deriving show, yojson]

let w_date a d = (a, d)
let strip (a, d) = a

type local = {
  s : int;
  m : int;
  h : int;
  day : int;
  month : int;
  year : int;
}

let now () = Unix.time ()

let of_local date =
  Unix.{
    tm_sec = date.s;
    tm_min = date.m;
    tm_hour = date.h;
    tm_mday = date.day;
    tm_mon = date.month - 1;
    tm_year = date.year - 1900;
    tm_wday = 0;
    tm_yday = 0;
    tm_isdst = false
  } |> Unix.mktime |> fst



let to_local t = 
  let tm = t |> Unix.localtime in
  Unix.{
    s = tm.tm_sec;
    m = tm.tm_min;
    h = tm.tm_hour;
    day = tm.tm_mday;
    month = tm.tm_mon+1;
    year = tm.tm_year + 1900
  }



let add d d' = d + d'
let diff d d' = d - d'
let compare d d' = Pervasives.compare d d'


(*module W_date = struct*)

(*let ndays_ago n w_date = *)


(*end*)
