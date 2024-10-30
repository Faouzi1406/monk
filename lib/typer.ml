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
  | _ -> todo ()

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
  | n, args ->
    Application { n; t = List.map (fun expr -> infer_expr ~env:e ~expr) args }
;;

let rec infer_stmt ~env:e = function
  | Ast.SVar v -> infer_let ~env:e v
  | SFunc f -> infer_func ~env:e f
  | SExpr expr -> infer_expr ~env:e ~expr
  | _ -> todo ()

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
and infer_func ~env:_ = function
  | n, _, params, stmt ->
    let b = new_tree () in
    let b = add_params ~env:b ~params in
    let t = infer_stmt ~env:b stmt in
    Abstraction { n; t; b = Some b }

and add_params ~env:e ~params:p =
  let rec add_params' ~env:e = function
    | (name, None) :: params ->
      let poly = Variable { n = name; t = Polymorphic } in
      add_params' ~env:(append_rule ~env:e ~rule:poly) params
    | (name, Some ty_name) :: params ->
      let ty = Ttree.type_var ~env:e ~name:ty_name in
      if ty = None then Tyerr.invalid_ty ~name:ty_name;
      let ty = Variable { n = name; t = ty_of ~env:e @@ Option.get ty } in
      add_params' ~env:(append_rule ~env:e ~rule:ty) params
    | [] -> e
  in
  add_params' ~env:e p
;;

let infer ~ast:t =
  let env = Ttree.new_tree () in
  let rec infer' ~env = function
    | stmt :: stmts ->
      let env = append_rule ~env ~rule:(infer_stmt ~env stmt) in
      infer' ~env stmts
    | [] -> env
  in
  match t with
  | Ast.Program (p, _) -> infer' ~env p
;;
