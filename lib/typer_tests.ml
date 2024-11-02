let parse src =
  let lexer = Lexing.from_string src in
  let lex buf = Lexer.read buf in
  Parser.main lex lexer
;;

let%expect_test "Basic inference" =
  let ast =
    parse
      {|
    let a = 1
    let b = "Yo"
    let c = "yo" + b
    let abc =  1.2
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
        Ttree.Let {n = "b"; t = Ttree.Variable {n = "string"; t = Ttree.String}};
        Ttree.Let {n = "c"; t = Ttree.Variable {n = "string"; t = Ttree.String}};
        Ttree.Let {n = "abc"; t = Ttree.Variable {n = "float"; t = Ttree.Float}}];
      prev = None }
    |}]
;;

let%expect_test "functions, params, types" =
  let ast =
    parse
      {|
    type square { x: int; y:int  }
    let v = {x: 20; y: 10}
    let hello_world(x) = {x: x; y: 10}
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
                 ("y", Ttree.Variable {n = "int"; t = Ttree.Int})])}};
        Ttree.Abstraction {n = "hello_world";
          p = [Ttree.Variable {n = "x"; t = Ttree.Int}];
          t =
          Ttree.Variable {n = "square";
            t =
            (Ttree.Object
               [("x", Ttree.Variable {n = "int"; t = Ttree.Int});
                 ("y", Ttree.Variable {n = "int"; t = Ttree.Int})])};
          b =
          (Some { Ttree.r = [Ttree.Variable {n = "x"; t = Ttree.Int}];
                  prev =
                  (Some { Ttree.r =
                          [Ttree.Variable {n = "string"; t = Ttree.String};
                            Ttree.Variable {n = "float"; t = Ttree.Float};
                            Ttree.Variable {n = "int"; t = Ttree.Int};
                            Ttree.Variable {n = "square";
                              t =
                              (Ttree.Object
                                 [("x", Ttree.Variable {n = "int"; t = Ttree.Int});
                                   ("y",
                                    Ttree.Variable {n = "int"; t = Ttree.Int})
                                   ])};
                            Ttree.Let {n = "v";
                              t =
                              Ttree.Variable {n = "square";
                                t =
                                (Ttree.Object
                                   [("x",
                                     Ttree.Variable {n = "int"; t = Ttree.Int});
                                     ("y",
                                      Ttree.Variable {n = "int"; t = Ttree.Int})
                                     ])}}
                            ];
                          prev = None })
                  })}
        ];
      prev = None }
    |}]
;;
