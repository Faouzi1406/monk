type data = (string, Tsymbol.t) Hashtbl.t

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

let init = function
  | KLocal prev ->
    TLocal { data = Hashtbl.create 20; scopes = Stack.create (); prev }
  | KGlobal -> TGlobal { data = Hashtbl.create 20; scopes = Stack.create () }
;;

let rec find value = function
  | TGlobal g -> Hashtbl.find_opt g.data value
  | TLocal g ->
    (match Hashtbl.find_opt g.data value with
     | Some v -> Some v
     | None -> find value g.prev)
;;

let append table = function
  | TGlobal g -> Stack.push table g.scopes
  | TLocal l -> Stack.push table l.scopes
;;

let add name value = function
  | TGlobal g -> Hashtbl.add g.data name value
  | TLocal l -> Hashtbl.add l.data name value
;;
