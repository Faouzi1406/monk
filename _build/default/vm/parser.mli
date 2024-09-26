
(* The type of tokens. *)

type token = 
  | SUB
  | SHOW
  | PUSH
  | POP
  | OR
  | NOTEQ
  | MUL
  | MOREEQ
  | MORE
  | LESSEQ
  | LESS
  | KOMMA
  | JMPZ
  | JMPNZ
  | INT of (int)
  | IDENT of (string)
  | FUNC
  | FLOAT of (float)
  | EQ
  | EOF
  | AND
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Instr.t list)
