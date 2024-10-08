type ast = Program of statement list * string option

and statement =
  | Expression of expression
  | Func of func
  | Var of var
  | Type of typef
  | Implement of implementf

and expression =
  | Literal of literal
  | Matches of matches
  | FuncValue of func_expr
  | BinaryOp of expression * op * expression
  | Condition of expression * comp * expression

and ident = string
and var = ident * ident option * expression
and arguments = expression list
and paramaters = ident * ident option list
and func = ident * ident option * paramaters * statement list
and func_expr = ident option * statement list
and case = Matchcase of ident * arguments option
and matches = case list * expression

and literal =
  | Int of int
  | Float of float
  | Ident of ident
  | String of string
  | Char of char
  | Array of expression list
  | Object of ident * expression list

and typef = ident * expression
and implementf = ident * statement list

and op =
  | Plus
  | Min
  | Div
  | Mul

and comp =
  | More
  | Less
  | MoreEq
  | LessEq
  | Eq
  | EqEq
