type t =
  | TyAnon of types
  | TyVar of
      { n : string
      ; mutable t : types
      }
  | TyCallable of
      { n : string
      ; mutable t : types
      }
[@@deriving show]

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
[@@deriving show]

let get_ty = function
  | TyAnon t -> t
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
