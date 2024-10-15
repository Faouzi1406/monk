type t =
  | TyVar of
      { n : string
      ; t : types
      }
  | TyCallable of
      { n : string
      ; t : types
      }

and types =
  | String
  | Float
  | Int
  | Boolean
  | Char
  | Ident of string
  | Void
  | Generic of string
  | Scheme of types list

let get_ty = function
  | TyVar t -> t.t
  | TyCallable t -> t.t
;;

let type_from_str s =
  match s with
  | "string" -> String
  | "float" -> Float
  | "int" -> Int
  | "bool" -> Boolean
  | "void" -> Void
  | n -> Ident n
;;
