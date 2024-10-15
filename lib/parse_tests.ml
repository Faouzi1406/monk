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
         let name(self: v): string =  { return self.fname }
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
                     [(Ast.SReturn
                         (Ast.SExpr
                            (Ast.EBinaryOp
                               ((Ast.ELiteral (Ast.LIdent "self")), Ast.ODot,
                                (Ast.ELiteral (Ast.LIdent "fname"))))))
                       ])))
               ]))
         ],
       None))
    |}]
;;
