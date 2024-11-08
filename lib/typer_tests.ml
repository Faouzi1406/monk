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
        Ttree.Variable {n = "bool"; t = Ttree.Bool};
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
    let hello_world(x) = 10
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
        Ttree.Variable {n = "bool"; t = Ttree.Bool};
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
          p = [Ttree.Variable {n = "x"; t = Ttree.Polymorphic}];
          t = Ttree.Variable {n = "int"; t = Ttree.Int};
          b =
          (Some { Ttree.r =
                  [Ttree.Variable {n = "x"; t = Ttree.Polymorphic};
                    Ttree.Variable {n = "int"; t = Ttree.Int}];
                  prev =
                  (Some { Ttree.r =
                          [Ttree.Variable {n = "string"; t = Ttree.String};
                            Ttree.Variable {n = "float"; t = Ttree.Float};
                            Ttree.Variable {n = "int"; t = Ttree.Int};
                            Ttree.Variable {n = "bool"; t = Ttree.Bool};
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

let%expect_test "Function block body" =
  let ast = parse {|
      let add_20(a) = {
      a + 20
      }
    |} in
  let infered = Typer.infer ~ast in
  print_string @@ Ttree.show_ttree infered;
  [%expect
    {|
    { Ttree.r =
      [Ttree.Variable {n = "string"; t = Ttree.String};
        Ttree.Variable {n = "float"; t = Ttree.Float};
        Ttree.Variable {n = "int"; t = Ttree.Int};
        Ttree.Variable {n = "bool"; t = Ttree.Bool};
        Ttree.Abstraction {n = "add_20";
          p = [Ttree.Variable {n = "a"; t = Ttree.Int}];
          t = Ttree.Variable {n = "a"; t = Ttree.Int};
          b =
          (Some { Ttree.r =
                  [Ttree.Variable {n = "a"; t = Ttree.Int};
                    Ttree.Variable {n = "a"; t = Ttree.Int}];
                  prev =
                  (Some { Ttree.r =
                          [Ttree.Variable {n = "string"; t = Ttree.String};
                            Ttree.Variable {n = "float"; t = Ttree.Float};
                            Ttree.Variable {n = "int"; t = Ttree.Int};
                            Ttree.Variable {n = "bool"; t = Ttree.Bool}];
                          prev = None })
                  })}
        ];
      prev = None }
    |}]
;;

let%expect_test "Function if_else controll flow" =
  let ast =
    parse
      {|
    let foo_or_bar(a) = if a == 5 {
      "this is working!"
    } 
    elif a == 2 {
      "yo is this working?"
    }
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
        Ttree.Variable {n = "bool"; t = Ttree.Bool};
        Ttree.Abstraction {n = "foo_or_bar";
          p = [Ttree.Variable {n = "a"; t = Ttree.Polymorphic}];
          t = Ttree.Variable {n = "string"; t = Ttree.String};
          b =
          (Some { Ttree.r =
                  [Ttree.Variable {n = "a"; t = Ttree.Polymorphic};
                    Ttree.Variable {n = "string"; t = Ttree.String};
                    Ttree.Variable {n = "string"; t = Ttree.String}];
                  prev =
                  (Some { Ttree.r =
                          [Ttree.Variable {n = "string"; t = Ttree.String};
                            Ttree.Variable {n = "float"; t = Ttree.Float};
                            Ttree.Variable {n = "int"; t = Ttree.Int};
                            Ttree.Variable {n = "bool"; t = Ttree.Bool}];
                          prev = None })
                  })}
        ];
      prev = None }
    |}]
;;

let%expect_test "Match function" =
  let ast =
    parse
      {|
      let fizz_buzz(x) = match x
      | 0 => "0"
      | 1 => "1"
      | x => "x is a number"

      let cool = "cool" +  fizz_buzz(10)
    |}
  in
  let infered = Typer.infer ~ast in
  print_string @@ Ttree.show_ttree infered;
  [%expect
    {|
    Ttree.Variable {n = "string"; t = Ttree.String}
    Ttree.Variable {n = "float"; t = Ttree.Float}
    Ttree.Variable {n = "int"; t = Ttree.Int}
    Ttree.Variable {n = "bool"; t = Ttree.Bool}
    Ttree.Abstraction {n = "fizz_buzz";
      p = [Ttree.Variable {n = "x"; t = Ttree.Int}];
      t = Ttree.Variable {n = "string"; t = Ttree.String};
      b =
      (Some { Ttree.r =
              [Ttree.Variable {n = "x"; t = Ttree.Int};
                Ttree.Variable {n = "string"; t = Ttree.String};
                Ttree.Variable {n = "string"; t = Ttree.String};
                Ttree.Variable {n = "string"; t = Ttree.String}];
              prev =
              (Some { Ttree.r =
                      [Ttree.Variable {n = "string"; t = Ttree.String};
                        Ttree.Variable {n = "float"; t = Ttree.Float};
                        Ttree.Variable {n = "int"; t = Ttree.Int};
                        Ttree.Variable {n = "bool"; t = Ttree.Bool}];
                      prev = None })
              })}
    { Ttree.r =
      [Ttree.Variable {n = "string"; t = Ttree.String};
        Ttree.Variable {n = "float"; t = Ttree.Float};
        Ttree.Variable {n = "int"; t = Ttree.Int};
        Ttree.Variable {n = "bool"; t = Ttree.Bool};
        Ttree.Abstraction {n = "fizz_buzz";
          p = [Ttree.Variable {n = "x"; t = Ttree.Int}];
          t = Ttree.Variable {n = "string"; t = Ttree.String};
          b =
          (Some { Ttree.r =
                  [Ttree.Variable {n = "x"; t = Ttree.Int};
                    Ttree.Variable {n = "string"; t = Ttree.String};
                    Ttree.Variable {n = "string"; t = Ttree.String};
                    Ttree.Variable {n = "string"; t = Ttree.String}];
                  prev =
                  (Some { Ttree.r =
                          [Ttree.Variable {n = "string"; t = Ttree.String};
                            Ttree.Variable {n = "float"; t = Ttree.Float};
                            Ttree.Variable {n = "int"; t = Ttree.Int};
                            Ttree.Variable {n = "bool"; t = Ttree.Bool}];
                          prev = None })
                  })};
        Ttree.Let {n = "cool";
          t = Ttree.Variable {n = "string"; t = Ttree.String}}
        ];
      prev = None }
    |}]
;;
