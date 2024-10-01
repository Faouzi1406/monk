open Value
open Instr

(*
   TODO:
   - Add better errors for type errors etc
   - Optimize certaint area's/speed up access to values
   - The design of this is not perfect just yet, we need to think about this better.
*)

exception END

type t =
  { mutable pc : int
  ; ds : value Stack.t
  ; il : Instr.t list
  ; labels : (string, int) Hashtbl.t
  ; globals : (int, value) Hashtbl.t
  }

let register_label t ~label:n ~pos:p = Hashtbl.add t.labels n p

let new_vm ~instructions:il =
  let vm =
    { ds = Stack.create ()
    ; il
    ; pc = 0
    ; globals = Hashtbl.create 200
    ; labels = Hashtbl.create 100
    }
  in
  List.iteri
    (fun i v ->
      match v with
      | Label name -> register_label vm ~label:name ~pos:i
      | _ -> ())
    il;
  vm
;;

let rec next t =
  try
    let instr = List.nth t.il t.pc in
    t.pc <- t.pc + 1;
    exec t instr
  with
  | Failure _ -> raise END

and exec t instr =
  match instr with
  | Instr.Show -> show t
  | Instr.Pop ->
    let _ = Stack.pop t.ds in
    ()
  | Instr.Push i -> Stack.push i t.ds
  | Instr.GotoZ a -> jmpz t ~arg:a
  | Instr.GotoNZ a -> jmpnz t ~arg:a
  | Instr.Alu op -> alu t ~op
  | Instr.Goto label -> goto t ~label
  | _ -> ()

and show t =
  match Stack.top t.ds with
  | Value.Int i -> print_int i
  | Value.Float fl -> print_float fl
  | Value.Ident id -> print_string id
  | Value.Object obj -> print_int obj

and jmpz t ~arg:a =
  let top = Stack.pop t.ds in
  match top with
  | Value.Int int -> if int == 0 then t.pc <- Hashtbl.find t.labels a
  | _ -> assert false

and jmpnz t ~arg:a =
  let top = Stack.pop t.ds in
  match top with
  | Value.Int int -> if int != 0 then t.pc <- Hashtbl.find t.labels a
  | _ -> assert false

and alu t ~op:a =
  match a with
  | Add ->
    let rhs = Stack.pop t.ds in
    let lhs = Stack.top t.ds in
    Stack.push lhs t.ds;
    let result = add ~lhs ~rhs in
    Stack.push result t.ds
  | Sub ->
    let rhs = Stack.pop t.ds in
    let lhs = Stack.top t.ds in
    Stack.push lhs t.ds;
    let result = sub ~lhs ~rhs in
    Stack.push result t.ds
  | Mul ->
    let rhs = Stack.pop t.ds in
    let lhs = Stack.top t.ds in
    Stack.push lhs t.ds;
    let result = mul ~lhs ~rhs in
    Stack.push result t.ds
  | And ->
    let rhs = Stack.pop t.ds in
    let lhs = Stack.top t.ds in
    Stack.push lhs t.ds;
    let result = cand ~lhs ~rhs in
    Stack.push result t.ds
  | Or ->
    let rhs = Stack.pop t.ds in
    let lhs = Stack.top t.ds in
    Stack.push lhs t.ds;
    let result = cor ~lhs ~rhs in
    Stack.push result t.ds
  | More ->
    let rhs = Stack.pop t.ds in
    let lhs = Stack.top t.ds in
    Stack.push lhs t.ds;
    let result = more ~lhs ~rhs in
    Stack.push result t.ds
  | MoreEq ->
    let rhs = Stack.pop t.ds in
    let lhs = Stack.top t.ds in
    Stack.push lhs t.ds;
    let result = moreeq ~lhs ~rhs in
    Stack.push result t.ds
  | Less ->
    let rhs = Stack.pop t.ds in
    let lhs = Stack.top t.ds in
    Stack.push lhs t.ds;
    let result = less ~lhs ~rhs in
    Stack.push result t.ds
  | LessEq ->
    let rhs = Stack.pop t.ds in
    let lhs = Stack.pop t.ds in
    let result = lesseq ~lhs ~rhs in
    Stack.push result t.ds
  | Eq ->
    let rhs = Stack.pop t.ds in
    let lhs = Stack.top t.ds in
    Stack.push lhs t.ds;
    let result = eq ~lhs ~rhs in
    Stack.push result t.ds
  | NotEq ->
    let rhs = Stack.pop t.ds in
    let lhs = Stack.top t.ds in
    Stack.push lhs t.ds;
    let result = neq ~lhs ~rhs in
    Stack.push result t.ds

and add ~lhs:l ~rhs:r =
  match l, r with
  | Value.Int lhs, Value.Int rhs -> Value.Int (lhs + rhs)
  | Value.Float lhs, Value.Float rhs -> Value.Float (lhs +. rhs)
  | _ -> assert false

and sub ~lhs:l ~rhs:r =
  match l, r with
  | Value.Int lhs, Value.Int rhs -> Value.Int (lhs - rhs)
  | Value.Float lhs, Value.Float rhs -> Value.Float (lhs -. rhs)
  | _ -> assert false

and mul ~lhs:l ~rhs:r =
  match l, r with
  | Value.Int lhs, Value.Int rhs -> Value.Int (lhs * rhs)
  | Value.Float lhs, Value.Float rhs -> Value.Float (lhs *. rhs)
  | _ -> assert false

and cand ~lhs:l ~rhs:r =
  match l, r with
  | Value.Int 1, Value.Int 1 -> Value.Int 1
  | Value.Int 1, Value.Int 0 -> Value.Int 0
  | Value.Int 0, Value.Int 1 -> Value.Int 0
  | Value.Int 0, Value.Int 0 -> Value.Int 0
  | _ -> assert false

and cor ~lhs:l ~rhs:r =
  match l, r with
  | Value.Int 1, Value.Int 1 -> Value.Int 1
  | Value.Int 1, Value.Int _ -> Value.Int 1
  | Value.Int 0, Value.Int 1 -> Value.Int 1
  | Value.Int 0, Value.Int 0 -> Value.Int 1
  | _ -> assert false

and more ~lhs:l ~rhs:r =
  match l, r with
  | Value.Int lhs, Value.Int rhs -> Value.Int (Bool.to_int (lhs > rhs))
  | Value.Float lhs, Value.Float rhs -> Value.Int (Bool.to_int (lhs > rhs))
  | _ -> assert false

and moreeq ~lhs:l ~rhs:r =
  match l, r with
  | Value.Int lhs, Value.Int rhs -> Value.Int (Bool.to_int (lhs >= rhs))
  | Value.Float lhs, Value.Float rhs -> Value.Int (Bool.to_int (lhs >= rhs))
  | _ -> assert false

and less ~lhs:l ~rhs:r =
  match l, r with
  | Value.Int lhs, Value.Int rhs -> Value.Int (Bool.to_int (lhs < rhs))
  | Value.Float lhs, Value.Float rhs -> Value.Int (Bool.to_int (lhs < rhs))
  | _ -> assert false

and lesseq ~lhs:l ~rhs:r =
  match l, r with
  | Value.Int lhs, Value.Int rhs -> Value.Int (Bool.to_int (lhs <= rhs))
  | Value.Float lhs, Value.Float rhs -> Value.Int (Bool.to_int (lhs <= rhs))
  | _ -> assert false

and eq ~lhs:l ~rhs:r =
  match l, r with
  | Value.Int lhs, Value.Int rhs -> Value.Int (Bool.to_int (lhs = rhs))
  | Value.Float lhs, Value.Float rhs -> Value.Int (Bool.to_int (lhs = rhs))
  | _ -> assert false

and neq ~lhs:l ~rhs:r =
  match l, r with
  | Value.Int lhs, Value.Int rhs -> Value.Int (Bool.to_int (lhs != rhs))
  | Value.Float lhs, Value.Float rhs -> Value.Int (Bool.to_int (lhs != rhs))
  | _ -> assert false

and goto t ~label:n =
  let pos = Hashtbl.find t.labels n in
  match List.nth t.il pos with
  | Label _ -> t.pc <- pos
  | _ -> assert false
;;

let lex src =
  let rec lex' list =
    match Lexer.read src with
    | Parser.EOF -> List.rev list
    | t -> lex' (t :: list)
  in
  lex' []
;;

let parse src =
  let lexer = Lexing.from_string src in
  let lex buf = Lexer.read buf in
  Parser.main lex lexer
;;

let%expect_test "Count to 900" =
  let instr =
    parse
      {|
    goto looped_count

    label looped_count:
      push 100
      show

      label adds:
      push 100
      add

      push 900
      eq
      gotonz exit

      pop
      show
      goto adds

    label exit:
      show
    |}
  in
  let vm = new_vm ~instructions:instr in
  try
    while true do
      next vm
    done
  with
  | END -> [%expect {| 100200300400500600700800900 |}]
;;
