open Ast
open Stack

type t = { gsymb : Stable.t }

let ident_ty env ident =
  match Stable.find ident env.gsymb with
  | Some v -> Tsymbol.get_ty v
  | None -> Tsymbol.Generic ident
;;

let rec infer_expr env = function
  | ECondition _ -> Tsymbol.Boolean
  | ECall c -> call_ty env c
  | ELiteral l -> lit_ty env l
  | EBinaryOp b -> bop_ty env b

and bop_ty env = function
  (*TODO: Handle .*)
  | lhs, _, _ -> infer_expr env lhs

and lit_ty env = function
  | LInt _ -> Tsymbol.Int
  | LFloat _ -> Float
  | LChar _ -> Char
  | LString _ -> String
  | LIdent id -> ident_ty env id
  | LArray _ -> assert false (*TODO: Arrays*)
  | LObject _ -> assert false (*TODO: Objects*)

and call_ty env = function
  | upon, _ -> ident_ty env upon
;;

let infer_var env = function
  | name, Some typed, _ ->
    Stable.add
      name
      (Tsymbol.TyVar { n = name; t = Tsymbol.type_from_str typed })
      env.gsymb
  | name, None, expr ->
    Stable.add
      name
      (Tsymbol.TyVar { n = name; t = infer_expr env expr })
      env.gsymb
;;

let infer_func env = function
  | _ -> assert false (*TODO: Local scope -> infer -> blablabla*)
;;

let infer_stmt e = function
  | SVar f -> infer_var e f
  | SFunc f -> infer_func e f
  | _ -> assert false
;;

let infer_symbols env ast =
  match ast with
  | Program (stmts, _) ->
    List.iter (fun s -> infer_stmt env s) stmts;
    env.gsymb
;;
