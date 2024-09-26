{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

(* part 1 *)
let int = '-'? ['0'-'9'] ['0'-'9']*

(* part 2 *)
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

(* part 3 *)
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* part 4 *)
rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "," { KOMMA }
  | "show"  { SHOW }
  | "pop"   { POP }
  | "push"  { PUSH }
  | "jmpz" { JMPZ }
  | "jmpnz" { JMPNZ }
  | "func"  { FUNC }
  | "func"  { FUNC }
  | "add"   { ADD }
  | "sub"   { SUB }
  | "mul"   { MUL }
  | "and"   { AND }
  | "or"    { OR }
  | "more"  { MORE }
  | "moreeq"  { MOREEQ }
  | "less"  { LESS }
  | "lesseq"  { LESSEQ }
  | "eq"    { EQ }
  | "neq"   { NOTEQ }
  | id       { IDENT (Lexing.lexeme lexbuf) }
  | eof      { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
