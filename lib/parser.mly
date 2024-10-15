%token <int> INT
%token <float> FLOAT
%token <string> IDENT
%token <string> STRING
%token RETURN
%token LET
%token IF
%token ELSE
%token ELIF
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
%token MOREEQ
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
%token DOT
%token PIPE
%token SEMICOLON
%token LEFTBRACE
%token RIGHTBRACE

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
        | v = var; { v }
        | t = typed; { SType(t) }
        | i = implement; { SImplement(i) }
        | f = func; { f }
        | e = expr { SExpr e }
        | c = controll_flow; { SControllFlow(c) }
        | b = block; { SBlock(b) }
        | r = ret; { r }


ret: RETURN; s = stmt; {SReturn s}
expr:
    | c = call; { c } 
    | b = b_op; { b }
    | c = cond; { c }
    | l = lit; { ELiteral l }


b_op:
        lhs = expr; o = op;  rhs = expr; { EBinaryOp(lhs, o, rhs) }

cond:
        lhs = expr; o = cop;  rhs = expr; { ECondition(lhs, o, rhs) }

call: 
            upon = IDENT; LEFTBRACE; a = separated_list(COMMA, expr); RIGHTBRACE; { ECall(upon, a)}

var: 
    LET; i = IDENT;  t = type_anot?; EQ;  e = expr { SVar (i, t, e) }

func: 
    LET; n = IDENT;  LEFTBRACE;  params = separated_list(COMMA, param); RIGHTBRACE; t = type_anot?;  EQ; s =  stmt;
    { SFunc(n, t, params, s) }


controll_flow:
    | m  = matching; {m}
    | i  = if_do; {i}
    
if_do:
    IF; e = expr;  s = stmt; el = list(elif); ed = else_do?; { CIf(e, s, el, ed) }

elif: 
    ELIF; e = expr;  s = stmt; {e, s}

else_do: 
        ELSE; s = stmt;  { s }

matching:
    MATCH; c = separated_list(PIPE, match_case); { CMatch(c) }

match_case: 
            e = expr; s = stmt; { e, s }

implement:
        IMPLEMENT; i = IDENT; LEFTCURLYBRACKET; m = list(stmt); RIGHTCURLYBRACKET; {i, m}

type_anot: 
    COLON; t = IDENT; { t }

typed: 
    TYPE; i = IDENT; e = expr; { i, e }

block: 
    LEFTCURLYBRACKET; s = list(stmt); RIGHTCURLYBRACKET; { s }

lit: 
  | i = INT; { LInt i }
  | f =  FLOAT; { LFloat f }
  | s = STRING; { LString  s }
  | i = IDENT;  { LIdent i }
  | o = obj; { o }
  | LEFTBRACKET; a = separated_list(SEMICOLON, expr); RIGHTBRACKET; { LArray a }

obj:
    LEFTCURLYBRACKET; fields = separated_list(SEMICOLON,field) ; RIGHTCURLYBRACKET; {LObject fields}

field:
    i = IDENT; COLON; e = expr; { i, e }

%inline op: 
            | ADD { OPlus }
            | MIN { OMin }
            | MUL { OMul }
            | DIV { ODiv }
            | DOT { ODot }


cop: 
  MORE { COMore }
  | LESS { COLess }
  | MOREEQ { COMoreEq}
  | LESSEQ { COLessEq } 
  | EQ { COLess }
  | EQEQ { COLess }


param:
        | lhs = IDENT; rhs =  type_anot?; { lhs, rhs }
