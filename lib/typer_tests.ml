let parse src =
  let lexer = Lexing.from_string src in
  let lex buf = Lexer.read buf in
  Parser.main lex lexer
;;

let%expect_test "Infer Let" =
  (*try*)
  let ast =
    parse
      {|
    let a = 1
    let b = 2
    let add = a + b
    let square(x) = x * 1
    |}
  in
  let infered = Typer.infer ~ast in
  print_string @@ Ttree.show_ttree infered;
  [%expect
    {|
    { Ttree.r =
      [Ttree.Variable {n = "string"; t = Ttree.String};
        Ttree.Variable {n = "float"; t = Ttree.Float};
        Ttree.Variable {n = "int"; t = Ttree.Int};
        Ttree.Let {n = "a"; t = Ttree.Variable {n = "int"; t = Ttree.Int}};
        Ttree.Let {n = "b"; t = Ttree.Variable {n = "int"; t = Ttree.Int}};
        Ttree.Let {n = "add";
          t = Ttree.Let {n = "a"; t = Ttree.Variable {n = "int"; t = Ttree.Int}}};
        Ttree.Abstraction {n = "square";
          t = Ttree.Variable {n = "x"; t = Ttree.Int};
          b =
          (Some { Ttree.r =
                  [Ttree.Variable {n = "string"; t = Ttree.String};
                    Ttree.Variable {n = "float"; t = Ttree.Float};
                    Ttree.Variable {n = "int"; t = Ttree.Int};
                    Ttree.Variable {n = "x"; t = Ttree.Int}]
                  })}
        ]
      }
    |}]
;;
(*with*)
(*| Tyerr.TyError t -> print_string @@ Tyerr.show_tyerror t*)
