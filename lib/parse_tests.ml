let parse src =
  let lexer = Lexing.from_string src in
  let lex buf = Lexer.read buf in
  Parser.main lex lexer
;;

let%expect_test "Parsing variable" =
  let v =
    parse {|
    let add(a, b) = a + b
    let other = add(10, 20)
    |}
  in
  print_string (Ast.show_ast v);
  [%expect
    {|
    (Ast.Program (
       [(Ast.SFunc
           ("add", None, [("a", None); ("b", None)],
            (Ast.SExpr
               (Ast.EBinaryOp
                  ((Ast.ELiteral (Ast.LIdent "a")), Ast.OPlus,
                   (Ast.ELiteral (Ast.LIdent "b")))))));
         (Ast.SVar
            ("other", None,
             (Ast.ECall
                ("add",
                 [(Ast.ELiteral (Ast.LInt 10)); (Ast.ELiteral (Ast.LInt 20))]))))
         ],
       None))
    |}]
;;

let%expect_test "Parsing type" =
  let v =
    parse
      {|
      type v { fname: string; lname: string }

      implement v {
         let name(self: v): string =  { self.fname }
      }
    |}
  in
  print_string (Ast.show_ast v);
  [%expect
    {|
    (Ast.Program (
       [(Ast.SType
           ("v",
            (Ast.ELiteral
               (Ast.LObject
                  [("fname", (Ast.ELiteral (Ast.LIdent "string")));
                    ("lname", (Ast.ELiteral (Ast.LIdent "string")))]))));
         (Ast.SImplement
            ("v",
             [(Ast.SFunc
                 ("name", (Some "string"), [("self", (Some "v"))],
                  (Ast.SBlock
                     [(Ast.SExpr
                         (Ast.EBinaryOp
                            ((Ast.ELiteral (Ast.LIdent "self")), Ast.ODot,
                             (Ast.ELiteral (Ast.LIdent "fname")))))
                       ])))
               ]))
         ],
       None))
    |}]
;;

let%expect_test "Parsing match" =
  let v =
    parse
      {|
      match "this"
      | "Hello world!" => { 
        let b = "10"
        b
      }
      | "this" => {
        "In truth this is the only case!"
      }
    |}
  in
  print_string (Ast.show_ast v);
  [%expect
    {|
    (Ast.Program (
       [(Ast.SControllFlow
           (Ast.CMatch ((Ast.ELiteral (Ast.LString "this")),
              [((Ast.ELiteral (Ast.LString "Hello world!")),
                (Ast.SBlock
                   [(Ast.SVar ("b", None, (Ast.ELiteral (Ast.LString "10"))));
                     (Ast.SExpr (Ast.ELiteral (Ast.LIdent "b")))]));
                ((Ast.ELiteral (Ast.LString "this")),
                 (Ast.SBlock
                    [(Ast.SExpr
                        (Ast.ELiteral
                           (Ast.LString "In truth this is the only case!")))
                      ]))
                ]
              )))
         ],
       None))
    |}]
;;
