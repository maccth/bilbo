module Bilbo.Tests.ParserTests.PrefixExpression

open Expecto
open Bilbo.Common.Ast
open Bilbo.Tests.ParserTests.Helpers


let prefixOpExprTests = [
    "a = &b", "a", PRE Amp (VAR "b"), "Get id var";
    "a = not b", "a", PRE Not (VAR "b"), "Not var";
    "a = not True", "a", PRE Not (true |> BOOL |> SExpr), "Not bool, true";
    "a = not False", "a", PRE Not (false |> BOOL |> SExpr), "Not bool, false";
]

[<Tests>]
let tests3 =
    testList "Prefix operator tests" (List.map exprTest prefixOpExprTests)