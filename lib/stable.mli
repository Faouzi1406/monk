(*Monk Symbol Table*)

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

val init : kind -> t
val find : string -> Tsymbol.sym_kind option -> t -> Tsymbol.t option
val append : t -> t -> unit
val add : Tsymbol.t -> t -> unit
val print_table : t -> unit
