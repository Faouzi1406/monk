open Value

type t =
  (*Show the top of the stack*)
  | Show
  | Pop
  (*Push value onto stack*)
  | Push of value
  (*Jump if zero*)
  | Jmpz of value
  (*Jump if not zero*)
  | Jmpnz of value
  | Alu of alu
  | Func of
      { name : string
      ; args : string list
      }

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
