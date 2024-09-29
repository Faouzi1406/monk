open Value
open Instr

exception END

module CallStack = struct
  type t =
    { ret : int
    ; ls : (string, value) Hashtbl.t
    ; prev : t option
    }
end

type t =
  { mutable pc : int
  ; cs : CallStack.t option
  ; ds : value Stack.t
  ; il : Instr.t list
  ; funcs : (string, int) Hashtbl.t
  ; globals : (int, value) Hashtbl.t
  }

let new_vm ~instructions:il =
  { cs = None
  ; ds = Stack.create ()
  ; il
  ; pc = 0
  ; globals = Hashtbl.create 200
  ; funcs = Hashtbl.create 100
  }
;;

let rec next t =
  try
    let instr = List.nth t.il t.pc in
    t.pc <- t.pc + 1;
    match instr with
    | Instr.Show -> show t
    | Instr.Pop ->
      let _ = Stack.pop t.ds in
      ()
    | Instr.Push i -> Stack.push i t.ds
    | Instr.Jmpz a -> jmpz t ~arg:a
    | Instr.Jmpnz a -> jmpnz t ~arg:a
    | Instr.Alu op -> alu t ~op
    | Instr.Func { name; _ } -> register_func t ~name
    | Instr.Call name -> call name
  with
  | Failure _ -> raise END

and show t =
  match Stack.top t.ds with
  | Value.Int i -> print_int i
  | Value.Float fl -> print_float fl
  | Value.Ident id -> print_string id
  | Value.Object obj -> print_int obj

and jmpz t ~arg:a =
  let top = Stack.pop t.ds in
  match top, a with
  | Value.Int int, Value.Int p -> if int == 0 then t.pc <- p
  | _ -> assert false

and jmpnz t ~arg:a =
  let top = Stack.pop t.ds in
  match top, a with
  | Value.Int int, Value.Int p -> if int != 0 then t.pc <- p
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

and register_func t ~name:n = Hashtbl.add t.funcs n t.pc
and call t ~name:n = 
  let pc = Hashtbl.find n

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

let%expect_test "Count to 80" =
  let instr =
    parse
      {|
    push 1
    push 1
    add

    push 80
    moreeq
    jmpz 8

    eq
    jmpnz 11

    pop
    push 0
    jmpz 1

    show
    |}
  in
  let vm = new_vm ~instructions:instr in
  try
    while true do
      next vm
    done
  with
  | END -> [%expect {| 80 |}]
;;
