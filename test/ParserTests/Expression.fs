module Bilbo.Tests.ParserTests.Expression

open Expecto
open Bilbo.Parser.Ast
open Bilbo.Parser.Parser
open FParsec

[<Tests>]
let tests =
  testList "Expression parsing tests" [
    testCase "Integer literal assignment" <| fun _ ->
      let progIntLiteral =
        """
        a = 10
        """ 
      let expProgIntLiteral =
        [
           Statement(
            ExpressionStatement(
              AssignmentExpression(
                "a",
                LiteralExpression(
                  IntLiteral(10)))))
        ]
      let parseOut = pBilboStr progIntLiteral |> function | Success(res, _, _) -> res
      Expect.equal expProgIntLiteral parseOut ""
      
    ]
    
