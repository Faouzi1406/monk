open Ast
open Stack

type t = { gsymb : Stable.t }

let ident_ty scope ident =
  match Stable.find ident scope with
  | Some v -> Tsymbol.get_ty v
  | None -> Tsymbol.Generic ident
;;

(*Assumed: that the params have already been loaded into local scope...*)
let callable_ty ~scope:s ~name:n ~params:p ~ret_ty:r =
  let scheme = List.map (fun (name, _) -> ident_ty s name) p in
  let ret_ty =
    match r with
    | Some t -> ident_ty s t
    | None -> Tsymbol.Generic n
  in
  let scheme = scheme @ [ ret_ty ] in
  Tsymbol.Scheme scheme
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

let infer_var scope = function
  | name, Some typed, _ ->
    Stable.add
      name
      (Tsymbol.TyVar { n = name; t = Tsymbol.type_from_str typed })
      scope
  | name, None, expr ->
    Stable.add
      name
      (Tsymbol.TyVar { n = name; t = infer_expr scope expr })
      scope
;;

let rec infer_func scope = function
  | name, ret_ty, params, body ->
    let scope = Stable.init @@ Stable.KLocal scope in
    load_params scope params;
    Stable.add
      name
      (Tsymbol.TyVar { n = name; t = callable_ty ~scope ~name ~params ~ret_ty })
      scope;
    assert false (*TODO: Local scope -> infer -> blablabla*)

and load_params scope params =
  List.iter
    (fun (name, typed) ->
      match typed with
      | Some t ->
        Stable.add name (Tsymbol.TyVar { n = name; t = ident_ty scope t }) scope
      | None ->
        Stable.add name (Tsymbol.TyVar { n = name; t = Generic name }) scope)
    params
;;

let infer_stmt e = function
  | SVar f -> infer_var e f
  | SFunc f -> infer_func e f
  | _ -> assert false
;;

let infer_symbols env ast =
  match ast with
  | Program (stmts, _) ->
    List.iter (fun s -> infer_stmt env.gsymb s) stmts;
    env.gsymb
;;
