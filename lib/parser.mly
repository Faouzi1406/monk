%token <int> INT
%token <float> FLOAT
%token <string> IDENT
%token <string> STRING
%token LET
%token IF
%token MATCH
%token TYPE
%token IMPLEMENT
%token IMPLEMENT
%token LEFTBRACKET
%token RIGHTBRACKET


%start <Ast.ast list> main

%{
    open Ast
%}


%%

main: 
    |  e = list(stmt); EOF;
    { Program (e, None) }

stmt:
        LET 


expr:
