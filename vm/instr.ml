open Value

type t =
  (*Show the top of the stack*)
  | Show
  (*Push value onto stack*)
  | Push of value
  | Pop
  (*Jump if zero*)
  | GotoZ of string
  (*Jump if not zero*)
  | GotoNZ of string
  | Alu of alu
  | Goto of string
  | Label of string

and alu =
  | Add
  | Sub
  | Mul
  | And
  | Or
  | More
  | MoreEq
  | Less
  | LessEq
  | Eq
  | NotEq
