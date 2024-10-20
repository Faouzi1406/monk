(**TODO: Really apply and callable should only be using t so that typevar is used as what it is a type variable*)

type t =
  | TyApply of
      { n : string
      ; mutable t : types
      }
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

and sym_kind =
  [ `TyApply
  | `TyVar
  | `TyCallable
  ]

let name_eq symb name sk =
  match symb, sk with
  | TyApply a, (None | Some `TyCallable) -> a.n = name
  | TyVar a, (None | Some `TyVar) -> a.n = name
  | TyCallable a, (None | Some `TyCallable) -> a.n = name
  | _ -> false
;;

let get_ty = function
  | TyApply t -> t.t
  | TyVar t -> t.t
  | TyCallable t -> t.t
;;

let type_from_str = function
  | "string" -> String
  | "float" -> Float
  | "int" -> Int
  | "bool" -> Boolean
  | "void" -> Void
  | n -> Ident n
;;
