%token <int> INT
%token <float> FLOAT
%token <string> IDENT
%token <string> STRING
%token LET
%token IF
%token MATCH
%token TYPE
%token IMPLEMENT
%token LEFTCURLYBRACKET
%token RIGHTCURLYBRACKET
%token LEFTBRACKET
%token RIGHTBRACKET
%token EQ
%token EQEQ
%token MORE
%token MOREQ
%token LESS
%token LESSEQ
%token ARROWRIGHT
%token ARROWLEFT
%token COMMA
%token COLON
%token ADD
%token MIN
%token MUL
%token DIV
%token EOF


%left PLUS MIN
%left MUL DIV

%start <Ast.ast> main

%{
    open Ast
%}


%%

main: 
    |  e = list(stmt); EOF;
    { Program (e, None) }


stmt:
        f = func; { f }
        | e = expr
        { SExpression e }
        | v = var;
        { v }


expr:
    | c = call; { c }
    | b = b_op; { b }

var: 
     LET; i = IDENT; t = IDENT?; EQ; e = expr
    { SVar (i, t, e) }

b_op:
        lhs = expr; o = op;  rhs = expr; { EBinaryOp(lhs, o, rhs) }

call: 
        upon = IDENT; a = separated_list(COMMA,expr); { ECall(upon, a)}

func: 
    params = separated_list(COMMA, param); ARROWRIGHT; LEFTCURLYBRACKET; stmts = list(stmt); RIGHTCURLYBRACKET
    { SFunc(params, stmts) }


op: 
            | ADD { OPlus }
            | MIN { OMin }
            | MUL { OMul }
            | DIV { ODiv }

param:
    | lhs = IDENT; COLON; rhs = IDENT?; { lhs, rhs }
