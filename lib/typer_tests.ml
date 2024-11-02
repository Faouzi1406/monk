let parse src =
  let lexer = Lexing.from_string src in
  let lex buf = Lexer.read buf in
  Parser.main lex lexer
;;

let%expect_test "Infer Let" =
  let ast =
    parse
      {|
    let a = 1
    let b = 2
    let add = a + b
    let square(x) = x * 1
    let squares(c, d) = c * c + d * d 
    let wow = squares(10, 10)
    let identity(x) = x

    type square { x: int; y:int  }
    let v = {x: 20; y: 10}
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
          p = [Ttree.Variable {n = "x"; t = Ttree.Int}];
          t = Ttree.Variable {n = "x"; t = Ttree.Int};
          b =
          (Some { Ttree.r =
                  [Ttree.Variable {n = "string"; t = Ttree.String};
                    Ttree.Variable {n = "float"; t = Ttree.Float};
                    Ttree.Variable {n = "int"; t = Ttree.Int};
                    Ttree.Variable {n = "x"; t = Ttree.Int}]
                  })};
        Ttree.Abstraction {n = "squares";
          p =
          [Ttree.Variable {n = "c"; t = Ttree.Int};
            Ttree.Variable {n = "d"; t = Ttree.Int}];
          t = Ttree.Variable {n = "c"; t = Ttree.Int};
          b =
          (Some { Ttree.r =
                  [Ttree.Variable {n = "string"; t = Ttree.String};
                    Ttree.Variable {n = "float"; t = Ttree.Float};
                    Ttree.Variable {n = "int"; t = Ttree.Int};
                    Ttree.Variable {n = "c"; t = Ttree.Int};
                    Ttree.Variable {n = "d"; t = Ttree.Int}]
                  })};
        Ttree.Let {n = "wow";
          t =
          Ttree.Application {n = "squares";
            t =
            [Ttree.Variable {n = "c"; t = Ttree.Int};
              Ttree.Variable {n = "d"; t = Ttree.Int}]}};
        Ttree.Abstraction {n = "identity";
          p = [Ttree.Variable {n = "x"; t = Ttree.Polymorphic}];
          t = Ttree.Variable {n = "x"; t = Ttree.Polymorphic};
          b =
          (Some { Ttree.r =
                  [Ttree.Variable {n = "string"; t = Ttree.String};
                    Ttree.Variable {n = "float"; t = Ttree.Float};
                    Ttree.Variable {n = "int"; t = Ttree.Int};
                    Ttree.Variable {n = "x"; t = Ttree.Polymorphic}]
                  })};
        Ttree.Variable {n = "square";
          t =
          (Ttree.Object
             [("x", Ttree.Variable {n = "int"; t = Ttree.Int});
               ("y", Ttree.Variable {n = "int"; t = Ttree.Int})])};
        Ttree.Let {n = "v";
          t =
          Ttree.Variable {n = "square";
            t =
            (Ttree.Object
               [("x", Ttree.Variable {n = "int"; t = Ttree.Int});
                 ("y", Ttree.Variable {n = "int"; t = Ttree.Int})])}}
        ]
      }
    |}]
;;
