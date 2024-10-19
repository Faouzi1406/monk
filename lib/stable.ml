type data = Tsymbol.t Stack.t

type t =
  | TGlobal of
      { data : data
      ; scopes : t Stack.t
      }
  | TLocal of
      { prev : t
      ; data : data
      ; scopes : t Stack.t
      }

type kind =
  | KLocal of t
  | KGlobal

exception Found_Symb of Tsymbol.t

let find_stack st name sk =
  try
    Stack.iter
      (fun v -> if Tsymbol.name_eq v name sk then raise (Found_Symb v))
      st;
    None
  with
  | Found_Symb symb -> Some symb
;;

let init = function
  | KLocal prev ->
    TLocal { data = Stack.create (); scopes = Stack.create (); prev }
  | KGlobal -> TGlobal { data = Stack.create (); scopes = Stack.create () }
;;

let rec find value sym_ty = function
  | TGlobal g -> find_stack g.data value sym_ty
  | TLocal g ->
    (match find_stack g.data value sym_ty with
     | Some v -> Some v
     | None -> find value sym_ty g.prev)
;;

let append table = function
  | TGlobal g -> Stack.push table g.scopes
  | TLocal l -> Stack.push table l.scopes
;;

let add value = function
  | TGlobal g -> Stack.push value g.data
  | TLocal l -> Stack.push value l.data
;;

let rec print_table = function
  | TGlobal g ->
    Stack.iter (fun symbol -> print_endline @@ Tsymbol.show symbol) g.data;
    Stack.iter (fun t -> print_table t) g.scopes
  | TLocal l ->
    Stack.iter (fun symbol -> print_endline @@ Tsymbol.show symbol) l.data;
    Stack.iter (fun t -> print_table t) l.scopes
;;
