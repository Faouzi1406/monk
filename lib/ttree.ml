type ttree =
  { r : rules list
  ; prev : ttree option
  }

and rules =
  | Type of { mutable t : types }
  | Variable of
      { n : string
      ; mutable t : types
      }
  | Application of
      { n : string
      ; t : rules list
      }
  | Abstraction of
      { n : string
      ; p : rules list
      ; t : rules
      ; b : ttree option
      }
  | Let of
      { n : string
      ; t : rules
      }
[@@deriving show]

and types =
  | Polymorphic
  | String
  | Float
  | Int
  | Ident
  | Object of (string * rules) list
  | Void
[@@deriving show]

let append_rule ~env:e ~rule:r = { r = e.r @ [ r ]; prev = e.prev }

let new_tree_default () =
  { r =
      [ Variable { n = "string"; t = String }
      ; Variable { n = "float"; t = Float }
      ; Variable { n = "int"; t = Int }
      ]
  ; prev = None
  }
;;

let new_tree prev = { r = []; prev }

let rule_is_named ~rule:r ~name:n =
  match r with
  | Type _ -> false
  | Variable r -> r.n = n
  | Application r -> r.n = n
  | Abstraction r -> r.n = n
  | Let r -> r.n = n
;;

let rec type_var ~env:e ~name:n =
  match
    Util.List.filter_first (fun v -> rule_is_named ~rule:v ~name:n) e.r, e.prev
  with
  | Some v, _ -> Some v
  | None, Some e -> type_var ~env:e ~name:n
  | None, _ -> None
;;

let rec ty_of ~env:e = function
  | Type { t } -> t
  | Variable { t; _ } -> t
  | Application { n; _ } ->
    (match type_var ~env:e ~name:n with
     | Some r -> ty_of ~env:e r
     | _ -> assert false)
  | Abstraction { t; _ } -> ty_of ~env:e t
  | Let { t; _ } -> ty_of ~env:e t
;;

let ty_name = function
  | Polymorphic -> "polymorphic"
  | String -> "string"
  | Float -> "float"
  | Int -> "int"
  | Ident -> "type"
  | Object _ -> "object"
;;

let rec rule_name ~env:e = function
  | Type { t } -> ty_name t
  | Variable { n; _ } -> n
  | Application { n; _ } ->
    (match type_var ~env:e ~name:n with
     | Some r -> rule_name ~env:e r
     | _ -> assert false)
  | Abstraction { t; _ } -> rule_name ~env:e t
  | Let { t; _ } -> rule_name ~env:e t
;;

let rec change_ty ~env:e ~new_ty:nty = function
  | Type t -> t.t <- ty_of ~env:e nty
  | Variable v -> v.t <- ty_of ~env:e nty
  | Application { n; _ } ->
    (match type_var ~env:e ~name:n with
     | Some r -> change_ty ~env:e ~new_ty:nty r
     | _ -> assert false)
  | Abstraction { t; _ } -> change_ty ~env:e ~new_ty:nty t
  | Let { t; _ } -> change_ty ~env:e ~new_ty:nty t
;;

let replace_poly ~env:e ~ty:t ~new_ty:rt =
  match ty_of ~env:e t with
  | Polymorphic -> change_ty ~env:e ~new_ty:rt t
  | _ -> ()
;;

exception NoMatch

let rec find_sig_match ~env:e ~ty:t =
  match List.find_opt (fun v -> sig_match (t, ty_of ~env:e v)) e.r, e.prev with
  | Some v, _ -> Some v
  | None, Some e -> find_sig_match ~env:e ~ty:t
  | _ -> None

and sig_match = function
  | Object lhs, Object rhs ->
    (try
       List.iter2
         (fun (left, _) (right, _) -> if left <> right then raise NoMatch)
         lhs
         rhs;
       List.length lhs = List.length rhs
     with
     | NoMatch -> false)
  | Int, Int -> true
  | String, String -> true
  | _ -> false
;;

let tail_rule ~env:e = Util.List.last e.r
