(*TODO:
  replace: if ty_of .. not .. -> check_ty_eq
*)

open Ttree

let todo () = raise (Invalid_argument "Not yet implemented")

let rec infer_expr ~env:e ~expr:ex =
  match ex with
  | Ast.ECall ex -> application ~env:e ex
  | EBinaryOp b -> bop ~env:e b
  | ELiteral l -> lit ~env:e l
  | _ -> todo ()

and lit ~env:e = function
  | Ast.LInt _ -> Option.get @@ type_var ~env:e ~name:"int"
  | LFloat _ -> Option.get @@ type_var ~env:e ~name:"float"
  | LString _ -> Option.get @@ type_var ~env:e ~name:"string"
  | LIdent id -> Option.get @@ type_var ~env:e ~name:id
  | LObject obj ->
    let obj =
      List.map (fun (name, expr) -> name, infer_expr ~env:e ~expr) obj
    in
    (match find_sig_match ~env:e ~ty:(Object obj) with
     | Some (Variable { n; t = Object obj_infr }) ->
       List.iter2
         (fun (_, lhs) (_, rhs) ->
           replace_poly ~env:e ~new_ty:lhs ~ty:rhs;
           replace_poly ~env:e ~new_ty:rhs ~ty:lhs;
           if ty_of ~env:e lhs <> ty_of ~env:e rhs
           then
             Tyerr.unequal_ty
               ~lhs:(rule_name ~env:e lhs)
               ~rhs:(rule_name ~env:e rhs))
         obj
         obj_infr;
       Variable { n; t = Object obj_infr }
     | Some v -> v
     | None -> Type { t = Object obj })
  | _ -> assert false

and bop ~env:e = function
  | _, Ast.ODot, _ -> todo ()
  | lhs, _, rhs ->
    let lhs = infer_expr ~env:e ~expr:lhs in
    let rhs = infer_expr ~env:e ~expr:rhs in
    replace_poly ~env:e ~new_ty:lhs ~ty:rhs;
    replace_poly ~env:e ~new_ty:rhs ~ty:lhs;
    if ty_of ~env:e lhs != ty_of ~env:e rhs
    then
      Tyerr.unequal_ty ~lhs:(rule_name ~env:e lhs) ~rhs:(rule_name ~env:e rhs)
    else lhs

and application ~env:e = function
  | name, args ->
    let v = type_var ~env:e ~name |> Option.get in
    (match v with
     | Abstraction { p; _ } ->
       List.iteri
         (fun index ty ->
           let expr = List.nth args index in
           let expr_ty = infer_expr ~env:e ~expr in
           replace_poly ~env:e ~ty ~new_ty:expr_ty;
           if ty_of ~env:e expr_ty != ty_of ~env:e ty
           then
             Tyerr.unequal_ty
               ~lhs:(rule_name ~env:e ty)
               ~rhs:(rule_name ~env:e expr_ty))
         p;
       Application { n = name; t = p }
     | _ ->
       assert false
       (*TODO: [ERROR] the user is calling something that isn't a function*))
;;

let rec infer_stmt ~env:e = function
  | Ast.SVar v -> append_rule ~env:e ~rule:(infer_let ~env:e v)
  | SFunc f -> append_rule ~env:e ~rule:(infer_func ~env:e f)
  | SExpr expr -> append_rule ~env:e ~rule:(infer_expr ~env:e ~expr)
  | SType decl_ty -> append_rule ~env:e ~rule:(add_decl_ty ~env:e decl_ty)
  | SBlock block -> infer_block ~env:e block
  | SControllFlow flow -> infer_flow ~env:e flow
  | _ -> todo ()

and infer_flow ~env:e = function
  | CIf (_, body, elif, _) ->
    let e = infer_stmt ~env:e body in
    let stmts = List.map (fun (_, body) -> body) elif in
    unify_stmts e stmts
  | _ -> todo ()

and unify_stmts env = function
  | stmt :: stmts ->
    let new_env = infer_stmt ~env stmt in
    let ty_lhs = Option.get @@ tail_rule ~env in
    let ty_rhs = Option.get @@ tail_rule ~env:new_env in
    print_string
    @@ "lhs = "
    ^ (show_types @@ ty_of ~env ty_lhs)
    ^ " rhs = "
    ^ show_types
    @@ ty_of ~env:new_env ty_rhs;
    if ty_of ~env ty_lhs <> ty_of ~env:new_env ty_rhs
    then
      Tyerr.unequal_ty ~lhs:(rule_name ~env ty_lhs) ~rhs:(rule_name ~env ty_rhs)
    else unify_stmts new_env stmts
  | [] -> env

and infer_block ~env:e = function
  | stmt :: [] ->
    let env = infer_stmt ~env:e stmt in
    env
  | stmt :: stmts ->
    let env = infer_stmt ~env:e stmt in
    infer_block ~env stmts
  | [] -> append_rule ~env:e ~rule:(Type { t = Void })

and add_decl_ty ~env:e = function
  | n, expr ->
    let t = infer_expr ~env:e ~expr in
    Variable { n; t = ty_of ~env:e t }

and infer_let ~env:e = function
  | n, Some ty_name, expr ->
    let ty = Ttree.type_var ~env:e ~name:ty_name in
    (match ty with
     | Some t ->
       let expr_ty = infer_expr ~env:e ~expr in
       if ty_of ~env:e expr_ty != ty_of ~env:e t
       then
         Tyerr.unequal_ty
           ~lhs:(rule_name ~env:e t)
           ~rhs:(rule_name ~env:e expr_ty)
       else Let { n; t }
     | None -> Tyerr.invalid_ty ~name:ty_name)
  | n, None, expr -> Let { n; t = infer_expr ~env:e ~expr }

(*TODO: We have to load in the global enviremont, otherwise inference will get pretty dang hard...*)
and infer_func ~env:e = function
  | n, _, params, stmt ->
    let b = new_tree @@ Some e in
    let p, b = add_params ~env:b ~params in
    let env = infer_stmt ~env:b stmt in
    Abstraction { n; t = Option.get @@ tail_rule ~env; p; b = Some env }

and add_params ~env:e ~params:p =
  let rec add_params' p e = function
    | (n, None) :: params ->
      let poly = Variable { n; t = Polymorphic } in
      let env = append_rule ~env:e ~rule:poly in
      add_params' (p @ [ poly ]) env params
    | (n, Some ty_name) :: params ->
      let ty = Ttree.type_var ~env:e ~name:ty_name in
      if ty = None then Tyerr.invalid_ty ~name:ty_name;
      let ty = Variable { n; t = ty_of ~env:e @@ Option.get ty } in
      let env = append_rule ~env:e ~rule:ty in
      add_params' (p @ [ ty ]) env params
    | [] -> p, e
  in
  add_params' [] e p
;;

let infer ~ast:t =
  let env = new_tree_default () in
  let rec infer' ~env = function
    | stmt :: stmts ->
      let env = infer_stmt ~env stmt in
      infer' ~env stmts
    | [] -> env
  in
  match t with
  | Ast.Program (p, _) -> infer' ~env p
;;
