%token <int> INT
%token <float> FLOAT
%token <string> IDENT
%token SHOW
%token POP
%token PUSH
%token JMPZ
%token JMPNZ
%token FUNC
%token ADD
%token SUB
%token MUL
%token AND
%token OR
%token MORE
%token MOREEQ
%token LESS
%token LESSEQ
%token EQ
%token NOTEQ
%token EOF
%token KOMMA

%start <Instr.t list> main

%{
    open Instr
%}


%%

main: 
    |  e = list(instr); EOF;
  { e }

instr:
  | SHOW; { Show }
  | POP; { Pop }
  | PUSH; e = value;  { Push e }
  | JMPZ; e = value;  { Jmpz e }
  | JMPNZ; e = value;  { Jmpnz e }
  | ADD;  { Alu Add }
  | SUB; { Alu Sub }
  | MUL; { Alu Mul  }
  | AND; { Alu And }
  | OR; { Alu Or }
  | MORE; { Alu More }
  | MOREEQ; { Alu MoreEq }
  | LESS; { Alu Less }
  | LESSEQ; { Alu LessEq }
  | EQ; { Alu Eq }
  | NOTEQ; { Alu NotEq }
  | FUNC; name = IDENT; args = separated_list(KOMMA, IDENT)  {Func {  name ; args }}

value: 
  | i = INT; { Value.Int i }
  | f = FLOAT; { Value.Float f }
  | i  = IDENT; {Value.Ident i}
