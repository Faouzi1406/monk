type ast = Program of statement list * string option [@@deriving show]

and statement =
  | SExpr of expression
  | SFunc of func
  | SVar of var
  | SType of typef
  | SImplement of implementf
  | SControllFlow of controll_flow
  | SBlock of statement list
  | SReturn of statement
[@@deriving show]

and expression =
  | ELiteral of literal
  | ECall of call
  | EBinaryOp of bin_op
  | ECondition of cond
[@@deriving show]

and ident = string [@@deriving show]
and var = ident * ident option * expression [@@deriving show]
and arguments = expression list [@@deriving show]
and paramaters = (ident * ident option) list [@@deriving show]
and func = ident * ident option * paramaters * statement
and call = ident * arguments [@@deriving show]
and bin_op = expression * op * expression [@@deriving show]
and cond = expression * comp * expression [@@deriving show]

and controll_flow =
  | CIf of if_cf
  | CMatch of (expression * statement) list
  | CWHile of expression * statement
[@@deriving show]

and if_cf =
  expression * statement * (expression * statement) list * statement option
[@@deriving show]

and literal =
  | LInt of int
  | LFloat of float
  | LIdent of ident
  | LString of string
  | LChar of char
  | LArray of expression list
  | LObject of (ident * expression) list
[@@deriving show]

and typef = ident * expression [@@deriving show]
and implementf = ident * statement list [@@deriving show]

and op =
  | OPlus
  | OMin
  | ODiv
  | OMul
  | ODot
[@@deriving show]

and comp =
  | COMore
  | COLess
  | COMoreEq
  | COLessEq
  | COEq
  | COEqEq
[@@deriving show]
