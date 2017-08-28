
open Ext_std

exception Value_error

module Type = struct

  type t =
    | Int
    | Float
    | Bool
    | String
    [@@deriving show, yojson]

  let int    = Int
  let float  = Float
  let bool   = Bool
  let string = String

end

module Raw = struct

  type t =
    | Int    of int
    | Float  of float
    | Bool   of bool
    | String of string
    [@@deriving show, yojson]

  let int    a = Int a
  let float  a = Float a
  let bool   a = Bool a
  let string a = String a

  let to_type = function
    | Int    _ -> Type.Int
    | Float  _ -> Type.Float
    | Bool   _ -> Type.Bool
    | String _ -> Type.String

  let to_string = function
    | Int    a -> string_of_int a
    | Float  a -> string_of_float a
    | Bool   a -> string_of_bool a
    | String a -> a
end

module Named = struct

  type 'a w_name = {
    name : bytes;
    data : 'a
  } [@@deriving show, yojson]

  type t =
    | Int    of int    w_name
    | Float  of float  w_name
    | Bool   of bool   w_name
    | String of string w_name
    [@@deriving show, yojson]

  let int    name data = Int    {name; data}
  let float  name data = Float  {name; data}
  let bool   name data = Bool   {name; data}
  let string name data = String {name; data}

  let name = function
    | Int    {name; data=_}
    | Float  {name; data=_}
    | Bool   {name; data=_}
    | String {name; data=_} -> name

  let to_raw = function
    | Int    {name=_; data} -> Raw.Int data
    | Float  {name=_; data} -> Raw.Float data
    | Bool   {name=_; data} -> Raw.Bool data
    | String {name=_; data} -> Raw.String data

  let to_type = function
    | Int    {name=_; data} -> Type.Int
    | Float  {name=_; data} -> Type.Float
    | Bool   {name=_; data} -> Type.Bool
    | String {name=_; data} -> Type.String

end


module Row = struct

  type 'a row = {
    name : bytes;
    cells : 'a list list;
    dates : Date.t list list;
    default : 'a;
  } [@@deriving show, yojson]

  let _create ~date name default n = {
    name;
    cells = List.ntimes n [default];
    dates = List.ntimes n [date];
    default;
  }

  let _name row = row.name
  let _dates row = row.dates

  let _add_cell ~date row = {
    name = row.name;
    cells = [row.default] :: row.cells;
    dates = [date] :: row.dates;
    default = row.default
  }

  let _del_cell n row = {
    name = row.name;
    cells = List.remi (fun i _ -> i = n) row.cells;
    dates = List.remi (fun i _ -> i = n) row.dates;
    default = row.default
  }

  let _push ~date n data row = 
    let insert v ll = 
      List.mapi (fun i l -> if i <> n then l else v :: l) ll
    in {
      name = row.name;
      cells = insert data row.cells;
      dates = insert date row.dates;
      default = row.default
    }


  let _get_cell n row = List.nth row.cells n
  let _get n row = _get_cell n row |> List.hd

  let _get_dates n row = List.nth row.dates n
  let _get_date n row  = _get_dates n row |> List.hd
    

  type t =
    | Int of int row
    | Float of float row
    | Bool of bool row 
    | String of bytes row
    [@@deriving show, yojson]

  let int ~date n name def = Int (_create ~date name def n)
  let float ~date n name def = Float (_create ~date name def n)
  let bool ~date n name def = Bool (_create ~date name def n)
  let string ~date n name def = String (_create ~date name def n)

  let of_raw ~date n name = function
    | Raw.Int    a -> Int    (_create ~date name a n)
    | Raw.Float  a -> Float  (_create ~date name a n)
    | Raw.Bool   a -> Bool   (_create ~date name a n)
    | Raw.String a -> String (_create ~date name a n)

  let to_type = function
    | Int _ -> Type.Int
    | Float _ -> Type.Float
    | Bool _ -> Type.Bool
    | String _ -> Type.String

  let name = function
    | Int a    -> _name a
    | Float a  -> _name a
    | Bool a   -> _name a
    | String a -> _name a

  let has_name n r = name r = n

  let dates = function
    | Int a    -> _dates a
    | Float a  -> _dates a
    | Bool a   -> _dates a
    | String a -> _dates a

  let add_cell ~date = function
    | Int    a -> Int    (_add_cell ~date a)
    | Float  a -> Float  (_add_cell ~date a)
    | Bool   a -> Bool   (_add_cell ~date a)
    | String a -> String (_add_cell ~date a)

  let del_cell n = function
    | Int    a -> Int    (_del_cell n a)
    | Float  a -> Float  (_del_cell n a)
    | Bool   a -> Bool   (_del_cell n a)
    | String a -> String (_del_cell n a)


  let push_raw ~date n v row = match row, v with
    | Int a,    Raw.Int v -> Int (_push ~date n v a)
    | Float a,  Raw.Float v -> Float (_push ~date n v a)
    | Bool a,   Raw.Bool v -> Bool (_push ~date n v a)
    | String a, Raw.String v -> String (_push ~date n v a)
    | _ -> raise Value_error

  let push_int ~date n v = function
    | Int a -> Int (_push ~date n v a)
    | _ -> raise Value_error

  let push_float ~date n v = function
    | Float a -> Float (_push ~date n v a)
    | _ -> raise Value_error

  let push_bool ~date n v = function
    | Bool a -> Bool (_push ~date n v a)
    | _ -> raise Value_error

  let push_string ~date n v = function
    | String a -> String (_push ~date n v a)
    | _ -> raise Value_error


  let get_int_cell n = function
    | Int a -> _get_cell n a
    | _ -> raise Value_error 

  let get_float_cell n = function
    | Float a -> _get_cell n a
    | _ -> raise Value_error 

  let get_bool_cell n = function
    | Bool a -> _get_cell n a
    | _ -> raise Value_error 

  let get_string_cell n = function
    | String a -> _get_cell n a
    | _ -> raise Value_error 


  let get_raw n = function
    | Int    a -> _get_cell n a |> List.hd |> Raw.int
    | Float  a -> _get_cell n a |> List.hd |> Raw.float
    | Bool   a -> _get_cell n a |> List.hd |> Raw.bool
    | String a -> _get_cell n a |> List.hd |> Raw.string


  let get_int    n row = get_int_cell    n row |> List.hd
  let get_float  n row = get_float_cell  n row |> List.hd
  let get_bool   n row = get_bool_cell   n row |> List.hd
  let get_string n row = get_string_cell n row |> List.hd

  let get_dates n row = List.nth (dates row) n
  let get_date  n row = get_dates n row |> List.hd

  let get_cell_w_date f n row =
    List.map2 Date.w_date (f n row) (get_dates n row) 

  let get_w_date f n row =
    Date.w_date (f n row) (get_dates n row)

  let get_int_cell_w_date    = get_cell_w_date get_int_cell
  let get_float_cell_w_date  = get_cell_w_date get_float_cell
  let get_bool_cell_w_date   = get_cell_w_date get_bool_cell
  let get_string_cell_w_date = get_cell_w_date get_string_cell

  let get_int_w_date    = get_w_date get_int
  let get_float_w_date  = get_w_date get_float
  let get_bool_w_date   = get_w_date get_bool
  let get_string_w_date = get_w_date get_string


end

