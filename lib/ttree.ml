type ttree = { r : rules list }

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
[@@deriving show]

let append_rule ~env:e ~rule:r = { r = e.r @ [ r ] }

let new_tree () =
  { r =
      [ Variable { n = "string"; t = String }
      ; Variable { n = "float"; t = Float }
      ; Variable { n = "int"; t = Int }
      ]
  }
;;

let rule_is_named ~rule:r ~name:n =
  match r with
  | Type _ -> false
  | Variable r -> r.n = n
  | Application r -> r.n = n
  | Abstraction r -> r.n = n
  | Let r -> r.n = n
;;

let type_var ~env:e ~name:n =
  Util.List.filter_first (fun v -> rule_is_named ~rule:v ~name:n) e.r
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
