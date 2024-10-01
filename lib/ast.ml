type ast = Program of statement list * string option

and statement =
  | Expression of expression
  | Fun of func
  | Var of var

and expression =
  | Literal of literal
  | Matches of matches
  | Func of func_expr

and ident = string
and var = ident * ident option * expression
and arguments = expression list
and func = ident * ident option * statement list
and func_expr = ident option * statement list
and case = Matchcase of ident * arguments option
and matches = case list * expression

and literal =
  | Int of int
  | Float of float
  | Ident of ident
  | String of string
  | Char of char
