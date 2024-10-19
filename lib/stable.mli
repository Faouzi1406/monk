(*Monk Symbol Table*)

type t

type kind =
  | KLocal of t
  | KGlobal

val init : kind -> t
val find : string -> t -> Tsymbol.t option
val append : t -> t -> unit
val add : string -> Tsymbol.t -> t -> unit
val print_table : t -> unit
