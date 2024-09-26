type value =
  | Int of int
  | Float of float
  | Ident of string
  | Object of int

module Object = struct
  type t = Array of value list
end
