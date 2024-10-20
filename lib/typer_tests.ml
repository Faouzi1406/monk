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
        let c = a
        c
      }
    let v = 10
    let b = 20
    let c = v + "hello there!"
    let d = add(20, 10)
  |}
  in
  let a = ast |> Typer.infer_symbols @@ Typer.init () in
  Stable.print_table a;
  [%expect
    {|
    Global:
       Tsymbol.TyVar {n = "d"; t = Tsymbol.Int}
       Tsymbol.TyApply {n = "add"; t = (Tsymbol.Scheme [Tsymbol.Int; Tsymbol.Int])}
       Tsymbol.TyVar {n = "c"; t = Tsymbol.String}
       Tsymbol.TyVar {n = "b"; t = Tsymbol.Int}
       Tsymbol.TyVar {n = "v"; t = Tsymbol.Int}
       Tsymbol.TyCallable {n = "add";
      t =
      (Tsymbol.Scheme
         [(Tsymbol.Generic "a"); (Tsymbol.Generic "b"); (Tsymbol.Generic "a")])}
    Local:
       Tsymbol.TyVar {n = "c"; t = (Tsymbol.Generic "a")}
    |}]
;;
