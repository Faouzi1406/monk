type t

val new_vm : instructions:Instr.t list -> t

(**Performs next instruction*)
val next : t -> unit
