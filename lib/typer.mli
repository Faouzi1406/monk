open Tsymbol
open Ast

type t

val init : unit -> t
val infer_symbols : t -> ast -> Stable.t
