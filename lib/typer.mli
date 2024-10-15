open Tsymbol
open Ast

type t

val infer_symbols : t -> ast -> Stable.t
