let parse src =
  let lexer = Lexing.from_string src in
  let lex buf = Lexer.read buf in
  Parser.main lex lexer
;;

let%expect_test "Test simple variables" =
  let ast =
    parse
      {|
      let add(a, b) = {
      a + b 
      }
    let v = 10
    let b = 20
    let c = v + b
    let d = add(20, 10)
  |}
  in
  let a = ast |> Typer.infer_symbols @@ Typer.init () in
  Stable.print_table a;
  [%expect
    {|
    Tsymbol.TyVar {n = "b"; t = Tsymbol.Int}
    Tsymbol.TyVar {n = "d"; t = (Tsymbol.Generic "b")}
    Tsymbol.TyCallable {n = "add";
      t =
      (Tsymbol.Scheme
         [(Tsymbol.Generic "a"); (Tsymbol.Generic "b"); (Tsymbol.Generic "b")])}
    Tsymbol.TyVar {n = "v"; t = Tsymbol.Int}
    Tsymbol.TyVar {n = "c"; t = Tsymbol.Int}
    |}]
;;
