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
  | '['      { LEFTBRACKET }
  | ']'      { RIGHTBRACKET }
  | '{'      { LEFTCURLYBRACKET }
  | '}'      { RIGHTCURLYBRACKET }
  | '('      { LEFTBRACE }
  | ')'      { RIGHTBRACE }
  | ';'      { SEMICOLON }
  | "let"  { LET }
  | "if"  { IF }
  | "else"  { ELSE }
  | "elif" { ELIF }
  | "match" { MATCH }
  | "type" { TYPE }
  | "implement"  { IMPLEMENT }
  | "return"  { RETURN }
  | '=' { EQ }
  | "==" { EQEQ }
  | '>' { MORE }
  | ">=" { MOREEQ }
  | '<' { LESS }
  | "<=" { LESSEQ }
  | "=>" { ARROWRIGHT }
  | ',' {COMMA}
  | ':' { COLON }
  | '+' { ADD }
  | '-' { MIN }
  | '*' { MUL }
  | '.' { DOT }
  | '/' { DIV }
  | '|' { PIPE }
  | ';' { SEMICOLON }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | id       { IDENT (Lexing.lexeme lexbuf) }
  
  | eof      { EOF }
  | '"'
     { let buffer = Buffer.create 1 in
       STRING (stringl buffer lexbuf)
     }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

 and  stringl buffer = parse
 | '"' { Buffer.contents buffer }
 | "\\t" { Buffer.add_char buffer '\t'; stringl buffer lexbuf }
 | "\\n" { Buffer.add_char buffer '\n'; stringl buffer lexbuf }
 | "\\n" { Buffer.add_char buffer '\n'; stringl buffer lexbuf }
 | '\\' '"' { Buffer.add_char buffer '"'; stringl buffer lexbuf }
 | '\\' '\\' { Buffer.add_char buffer '\\'; stringl buffer lexbuf }
 | eof { raise End_of_file }
 | _ as char { Buffer.add_char buffer char; stringl buffer lexbuf }
