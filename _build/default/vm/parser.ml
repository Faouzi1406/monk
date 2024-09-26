
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | SUB
    | SHOW
    | PUSH
    | POP
    | OR
    | NOTEQ
    | MUL
    | MOREEQ
    | MORE
    | LESSEQ
    | LESS
    | KOMMA
    | JMPZ
    | JMPNZ
    | INT of (
# 1 "vm/parser.mly"
       (int)
# 29 "vm/parser.ml"
  )
    | IDENT of (
# 3 "vm/parser.mly"
       (string)
# 34 "vm/parser.ml"
  )
    | FUNC
    | FLOAT of (
# 2 "vm/parser.mly"
       (float)
# 40 "vm/parser.ml"
  )
    | EQ
    | EOF
    | AND
    | ADD
  
end

include MenhirBasics

# 26 "vm/parser.mly"
  
    open Instr

# 55 "vm/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState00 : ('s, _menhir_box_main) _menhir_state
    (** State 00.
        Stack shape : .
        Start symbol: main. *)

  | MenhirState03 : (('s, _menhir_box_main) _menhir_cell1_PUSH, _menhir_box_main) _menhir_state
    (** State 03.
        Stack shape : PUSH.
        Start symbol: main. *)

  | MenhirState16 : (('s, _menhir_box_main) _menhir_cell1_JMPZ, _menhir_box_main) _menhir_state
    (** State 16.
        Stack shape : JMPZ.
        Start symbol: main. *)

  | MenhirState18 : (('s, _menhir_box_main) _menhir_cell1_JMPNZ, _menhir_box_main) _menhir_state
    (** State 18.
        Stack shape : JMPNZ.
        Start symbol: main. *)

  | MenhirState21 : (('s, _menhir_box_main) _menhir_cell1_FUNC _menhir_cell0_IDENT, _menhir_box_main) _menhir_state
    (** State 21.
        Stack shape : FUNC IDENT.
        Start symbol: main. *)

  | MenhirState23 : (('s, _menhir_box_main) _menhir_cell1_IDENT, _menhir_box_main) _menhir_state
    (** State 23.
        Stack shape : IDENT.
        Start symbol: main. *)

  | MenhirState33 : (('s, _menhir_box_main) _menhir_cell1_instr, _menhir_box_main) _menhir_state
    (** State 33.
        Stack shape : instr.
        Start symbol: main. *)


and ('s, 'r) _menhir_cell1_instr = 
  | MenhirCell1_instr of 's * ('s, 'r) _menhir_state * (Instr.t)

and ('s, 'r) _menhir_cell1_FUNC = 
  | MenhirCell1_FUNC of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_IDENT = 
  | MenhirCell1_IDENT of 's * ('s, 'r) _menhir_state * (
# 3 "vm/parser.mly"
       (string)
# 104 "vm/parser.ml"
)

and 's _menhir_cell0_IDENT = 
  | MenhirCell0_IDENT of 's * (
# 3 "vm/parser.mly"
       (string)
# 111 "vm/parser.ml"
)

and ('s, 'r) _menhir_cell1_JMPNZ = 
  | MenhirCell1_JMPNZ of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_JMPZ = 
  | MenhirCell1_JMPZ of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_PUSH = 
  | MenhirCell1_PUSH of 's * ('s, 'r) _menhir_state

and _menhir_box_main = 
  | MenhirBox_main of (Instr.t list) [@@unboxed]

let _menhir_action_01 =
  fun () ->
    (
# 38 "vm/parser.mly"
          ( Show )
# 131 "vm/parser.ml"
     : (Instr.t))

let _menhir_action_02 =
  fun () ->
    (
# 39 "vm/parser.mly"
         ( Pop )
# 139 "vm/parser.ml"
     : (Instr.t))

let _menhir_action_03 =
  fun e ->
    (
# 40 "vm/parser.mly"
                      ( Push e )
# 147 "vm/parser.ml"
     : (Instr.t))

let _menhir_action_04 =
  fun e ->
    (
# 41 "vm/parser.mly"
                      ( Jmpz e )
# 155 "vm/parser.ml"
     : (Instr.t))

let _menhir_action_05 =
  fun e ->
    (
# 42 "vm/parser.mly"
                       ( Jmpnz e )
# 163 "vm/parser.ml"
     : (Instr.t))

let _menhir_action_06 =
  fun () ->
    (
# 43 "vm/parser.mly"
          ( Alu Add )
# 171 "vm/parser.ml"
     : (Instr.t))

let _menhir_action_07 =
  fun () ->
    (
# 44 "vm/parser.mly"
         ( Alu Sub )
# 179 "vm/parser.ml"
     : (Instr.t))

let _menhir_action_08 =
  fun () ->
    (
# 45 "vm/parser.mly"
         ( Alu Mul  )
# 187 "vm/parser.ml"
     : (Instr.t))

let _menhir_action_09 =
  fun () ->
    (
# 46 "vm/parser.mly"
         ( Alu And )
# 195 "vm/parser.ml"
     : (Instr.t))

let _menhir_action_10 =
  fun () ->
    (
# 47 "vm/parser.mly"
        ( Alu Or )
# 203 "vm/parser.ml"
     : (Instr.t))

let _menhir_action_11 =
  fun () ->
    (
# 48 "vm/parser.mly"
          ( Alu More )
# 211 "vm/parser.ml"
     : (Instr.t))

let _menhir_action_12 =
  fun () ->
    (
# 49 "vm/parser.mly"
            ( Alu MoreEq )
# 219 "vm/parser.ml"
     : (Instr.t))

let _menhir_action_13 =
  fun () ->
    (
# 50 "vm/parser.mly"
          ( Alu Less )
# 227 "vm/parser.ml"
     : (Instr.t))

let _menhir_action_14 =
  fun () ->
    (
# 51 "vm/parser.mly"
            ( Alu LessEq )
# 235 "vm/parser.ml"
     : (Instr.t))

let _menhir_action_15 =
  fun () ->
    (
# 52 "vm/parser.mly"
        ( Alu Eq )
# 243 "vm/parser.ml"
     : (Instr.t))

let _menhir_action_16 =
  fun () ->
    (
# 53 "vm/parser.mly"
           ( Alu NotEq )
# 251 "vm/parser.ml"
     : (Instr.t))

let _menhir_action_17 =
  fun name xs ->
    let args = 
# 241 "<standard.mly>"
    ( xs )
# 259 "vm/parser.ml"
     in
    (
# 54 "vm/parser.mly"
                                                             (Func {  name ; args })
# 264 "vm/parser.ml"
     : (Instr.t))

let _menhir_action_18 =
  fun () ->
    (
# 216 "<standard.mly>"
    ( [] )
# 272 "vm/parser.ml"
     : (Instr.t list))

let _menhir_action_19 =
  fun x xs ->
    (
# 219 "<standard.mly>"
    ( x :: xs )
# 280 "vm/parser.ml"
     : (Instr.t list))

let _menhir_action_20 =
  fun () ->
    (
# 145 "<standard.mly>"
    ( [] )
# 288 "vm/parser.ml"
     : (string list))

let _menhir_action_21 =
  fun x ->
    (
# 148 "<standard.mly>"
    ( x )
# 296 "vm/parser.ml"
     : (string list))

let _menhir_action_22 =
  fun e ->
    (
# 35 "vm/parser.mly"
  ( e )
# 304 "vm/parser.ml"
     : (Instr.t list))

let _menhir_action_23 =
  fun x ->
    (
# 250 "<standard.mly>"
    ( [ x ] )
# 312 "vm/parser.ml"
     : (string list))

let _menhir_action_24 =
  fun x xs ->
    (
# 253 "<standard.mly>"
    ( x :: xs )
# 320 "vm/parser.ml"
     : (string list))

let _menhir_action_25 =
  fun i ->
    (
# 57 "vm/parser.mly"
             ( Value.Int i )
# 328 "vm/parser.ml"
     : (Value.value))

let _menhir_action_26 =
  fun f ->
    (
# 58 "vm/parser.mly"
               ( Value.Float f )
# 336 "vm/parser.ml"
     : (Value.value))

let _menhir_action_27 =
  fun i ->
    (
# 59 "vm/parser.mly"
                (Value.Ident i)
# 344 "vm/parser.ml"
     : (Value.value))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ADD ->
        "ADD"
    | AND ->
        "AND"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | FLOAT _ ->
        "FLOAT"
    | FUNC ->
        "FUNC"
    | IDENT _ ->
        "IDENT"
    | INT _ ->
        "INT"
    | JMPNZ ->
        "JMPNZ"
    | JMPZ ->
        "JMPZ"
    | KOMMA ->
        "KOMMA"
    | LESS ->
        "LESS"
    | LESSEQ ->
        "LESSEQ"
    | MORE ->
        "MORE"
    | MOREEQ ->
        "MOREEQ"
    | MUL ->
        "MUL"
    | NOTEQ ->
        "NOTEQ"
    | OR ->
        "OR"
    | POP ->
        "POP"
    | PUSH ->
        "PUSH"
    | SHOW ->
        "SHOW"
    | SUB ->
        "SUB"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_31 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_main =
    fun _menhir_stack _v ->
      let e = _v in
      let _v = _menhir_action_22 e in
      MenhirBox_main _v
  
  let rec _menhir_run_34 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_instr -> _ -> _menhir_box_main =
    fun _menhir_stack _v ->
      let MenhirCell1_instr (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_19 x xs in
      _menhir_goto_list_instr_ _menhir_stack _v _menhir_s
  
  and _menhir_goto_list_instr_ : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState33 ->
          _menhir_run_34 _menhir_stack _v
      | MenhirState00 ->
          _menhir_run_31 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_01 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_07 () in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_instr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_instr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SUB ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
      | SHOW ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
      | PUSH ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
      | POP ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
      | OR ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
      | NOTEQ ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
      | MUL ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
      | MOREEQ ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
      | MORE ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
      | LESSEQ ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
      | LESS ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
      | JMPZ ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
      | JMPNZ ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
      | FUNC ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
      | EQ ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
      | AND ->
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
      | ADD ->
          _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState33
      | EOF ->
          let _v_0 = _menhir_action_18 () in
          _menhir_run_34 _menhir_stack _v_0
      | _ ->
          _eRR ()
  
  and _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_01 () in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PUSH (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState03 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | INT _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_04 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let i = _v in
      let _v = _menhir_action_25 i in
      _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_value : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState18 ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState16 ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState03 ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_19 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_JMPNZ -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_JMPNZ (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_05 e in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_17 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_JMPZ -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_JMPZ (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_04 e in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_07 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_PUSH -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_PUSH (_menhir_stack, _menhir_s) = _menhir_stack in
      let e = _v in
      let _v = _menhir_action_03 e in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_05 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let i = _v in
      let _v = _menhir_action_27 i in
      _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_06 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let f = _v in
      let _v = _menhir_action_26 f in
      _menhir_goto_value _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_08 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_02 () in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_09 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_10 () in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_10 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_16 () in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_11 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_08 () in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_12 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_12 () in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_13 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_11 () in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_14 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_14 () in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_15 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_13 () in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_16 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_JMPZ (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState16 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | INT _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_18 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_JMPNZ (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState18 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | INT _v ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | IDENT _v ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_20 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FUNC (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | IDENT _v ->
          let _menhir_stack = MenhirCell0_IDENT (_menhir_stack, _v) in
          let _menhir_s = MenhirState21 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | ADD | AND | EOF | EQ | FUNC | JMPNZ | JMPZ | LESS | LESSEQ | MORE | MOREEQ | MUL | NOTEQ | OR | POP | PUSH | SHOW | SUB ->
              let _v = _menhir_action_20 () in
              _menhir_goto_loption_separated_nonempty_list_KOMMA_IDENT__ _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_22 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | KOMMA ->
          let _menhir_stack = MenhirCell1_IDENT (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState23 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | IDENT _v ->
              _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | ADD | AND | EOF | EQ | FUNC | JMPNZ | JMPZ | LESS | LESSEQ | MORE | MOREEQ | MUL | NOTEQ | OR | POP | PUSH | SHOW | SUB ->
          let x = _v in
          let _v = _menhir_action_23 x in
          _menhir_goto_separated_nonempty_list_KOMMA_IDENT_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_separated_nonempty_list_KOMMA_IDENT_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState21 ->
          _menhir_run_25 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState23 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_25 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_FUNC _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let x = _v in
      let _v = _menhir_action_21 x in
      _menhir_goto_loption_separated_nonempty_list_KOMMA_IDENT__ _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_loption_separated_nonempty_list_KOMMA_IDENT__ : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_FUNC _menhir_cell0_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell0_IDENT (_menhir_stack, name) = _menhir_stack in
      let MenhirCell1_FUNC (_menhir_stack, _menhir_s) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_17 name xs in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_24 : type  ttv_stack. (ttv_stack, _menhir_box_main) _menhir_cell1_IDENT -> _ -> _ -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_IDENT (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_24 x xs in
      _menhir_goto_separated_nonempty_list_KOMMA_IDENT_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_27 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_15 () in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_28 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_09 () in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_29 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_main) _menhir_state -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_06 () in
      _menhir_goto_instr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_main =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SUB ->
          _menhir_run_01 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | SHOW ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | PUSH ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | POP ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | OR ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | NOTEQ ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | MUL ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | MOREEQ ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | MORE ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | LESSEQ ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | LESS ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | JMPZ ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | JMPNZ ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | FUNC ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | EQ ->
          _menhir_run_27 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | AND ->
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | ADD ->
          _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState00
      | EOF ->
          let _v = _menhir_action_18 () in
          _menhir_run_31 _menhir_stack _v
      | _ ->
          _eRR ()
  
end

let main =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_main v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
