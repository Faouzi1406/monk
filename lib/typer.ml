open Ast
open Stack

type t = { gsymb : Stable.t }

let init () = { gsymb = Stable.init KGlobal }

let rec last_ty = function
  | [ x ] -> x
  | [] -> Tsymbol.Void
  | _ :: rest -> last_ty rest
;;

let ident_ty scope ident =
  match Stable.find ident scope with
  | Some v -> Tsymbol.get_ty v
  | None -> Tsymbol.Generic ident
;;

let rec infer_expr ~scope:s ~expr:e =
  match e with
  | ECondition _ -> Tsymbol.Boolean
  | ECall c -> call_ty s c
  | ELiteral l -> lit_ty s l
  | EBinaryOp b -> bop_ty s b

and bop_ty scope = function
  (*TODO: We should probably think much much better about how we infer the type here.
    Also: lhs '.'  rhs, does not mean that type(lhs) === type(rhs) *)
  | _, _, expr -> infer_expr ~scope ~expr

and lit_ty env = function
  | LInt _ -> Tsymbol.Int
  | LFloat _ -> Float
  | LChar _ -> Char
  | LString _ -> String
  | LIdent id -> ident_ty env id
  | LArray _ -> assert false (*TODO: Arrays*)
  | LObject _ -> assert false (*TODO: Objects*)

(**TODO: This should never happend roight?*)
and call_ty env = function
  | upon, _ ->
    (match ident_ty env upon with
     | Tsymbol.Scheme s -> List.nth s @@ (List.length s - 1)
     | _ -> assert false)
;;

let rec infer_stmt ~scope:s ~stmt:st =
  match st with
  | SExpr expr -> infer_expr ~scope:s ~expr
  | SVar var -> infer_var s var
  | SBlock block -> infer_block s block
  | SFunc func -> infer_func s func

and infer_var s = function
  | name, _, expr ->
    let ty = infer_expr ~scope:s ~expr in
    s |> Stable.add name @@ TyVar { n = name; t = ty };
    ty

and infer_func s = function
  | name, typed, params, stmt ->
    let fn_scope = Stable.init @@ KLocal s in
    let ty = infer_stmt ~scope:fn_scope ~stmt in
    let params =
      List.map
        (fun (name, t) ->
          (match t with
           | Some t -> t
           | None -> name)
          |> ident_ty fn_scope)
        params
    in
    Stable.append s fn_scope;
    let ft = Tsymbol.Scheme (params @ [ ty ]) in
    s |> Stable.add name @@ Tsymbol.TyCallable { n = name; t = ft };
    ft

(**TODO: Implement Type inference for return ...*)
and infer_block s block =
  last_ty @@ List.map (fun stmt -> infer_stmt ~scope:s ~stmt) block
;;

let infer_symbols env ast =
  match ast with
  | Program (stmts, _) ->
    List.iter
      (fun stmt ->
        let _ = infer_stmt ~scope:env.gsymb ~stmt in
        ())
      stmts;
    env.gsymb
;;
