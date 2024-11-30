type tyerror =
  | InvalidType of string
  | UnequalTypes of string * string
[@@deriving show]

exception TyError of tyerror

let invalid_ty ~name:n = raise (TyError (InvalidType n))
let unequal_ty ~lhs:l ~rhs:r = raise (TyError (UnequalTypes (l, r)))
