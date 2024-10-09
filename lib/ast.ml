type ast = Program of statement list * string option

and statement =
  | SExpression of expression
  | SFunc of func
  | SVar of var
  | SType of typef
  | SImplement of implementf
  | SControllFlow of controll_flow
  | SBlock of statement list

and expression =
  | ELiteral of literal
  | ECall of call
  | EBinaryOp of bin_op
  | ECondition of cond

and ident = string
and var = ident * ident option * expression
and arguments = expression list
and paramaters = (ident * ident option) list
and func = paramaters * statement list
and call = ident * arguments
and bin_op = expression * op * expression
and cond = expression * comp * expression

and controll_flow =
  | CIf of if_cf
  | CMatch of (expression * statement) list
  | CWHile of expression * statement

and if_cf =
  expression * statement * (expression * statement list) * statement option

and literal =
  | LInt of int
  | LFloat of float
  | LIdent of ident
  | LString of string
  | LChar of char
  | LArray of expression list
  | LObject of (ident * expression) list

and typef = ident * expression
and implementf = (ident * statement) list

and op =
  | OPlus
  | OMin
  | ODiv
  | OMul

and comp =
  | COMore
  | COLess
  | COMoreEq
  | COLessEq
  | COEq
  | COEqEq
