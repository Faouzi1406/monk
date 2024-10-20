open Ast
open Stack

type t = { gsymb : Stable.t }

let init () = { gsymb = Stable.init KGlobal }

let rec last_ty = function
  | [ x ] -> x
  | [] -> Tsymbol.Void
  | _ :: rest -> last_ty rest
;;

let ident_ty scope ident sk =
  match Stable.find ident sk scope with
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
  | LIdent id -> ident_ty env id None
  | LArray _ -> assert false (*TODO: Arrays*)
  | LObject _ -> assert false (*TODO: Objects*)

(**TODO: This should never happend roight?*)
and call_ty scope = function
  | upon, args ->
    let apply = List.map (fun expr -> infer_expr ~scope ~expr) args in
    scope |> Stable.add @@ Tsymbol.TyApply { n = upon; t = Scheme apply };
    (match ident_ty scope upon @@ Some `TyCallable with
     | Tsymbol.Scheme s ->
       let ret_ty = List.nth s @@ (List.length s - 1) in
       ret_ty
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
    s |> Stable.add @@ TyVar { n = name; t = ty };
    ty

and infer_func s = function
  | name, typed, params, stmt ->
    let fn_scope = Stable.init @@ KLocal s in
    let ty = infer_stmt ~scope:fn_scope ~stmt in
    let params =
      List.map
        (fun (name, t) ->
          match t with
          | Some t -> ident_ty fn_scope t None
          | None -> Generic name)
        params
    in
    Stable.append fn_scope s;
    let ft = Tsymbol.Scheme (params @ [ ty ]) in
    s |> Stable.add @@ Tsymbol.TyCallable { n = name; t = ft };
    ft

(**TODO: Implement Type inference for return ...*)
and infer_block s block =
  last_ty @@ List.map (fun stmt -> infer_stmt ~scope:s ~stmt) block
;;

let rec substitution table =
  List.iter (fun v -> sub) table.data;
  ()

and substitute table symbol =
  match symbol with
  | Tsymbol.TyApply a -> assert false
  | TyCallable b -> assert false
  | TyVar v -> assert false
;;

let infer_symbols env ast =
  match ast with
  | Program (stmts, _) ->
    List.iter
      (fun stmt ->
        let _ = infer_stmt ~scope:env.gsymb ~stmt in
        ())
      stmts;
    substitution env.gsymb;
    env.gsymb
;;
