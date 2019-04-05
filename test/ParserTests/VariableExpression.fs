module Bilbo.Tests.ParserTests.VariableExpression

open Expecto
open Bilbo.Parser.Ast
open Bilbo.Tests.ParserTests.Helpers

let dotAccessTests = [
    "a = obj.b", "a", DOT (VAR "obj") "b", "Dot access, one";
]

let vExprTest = consAssignTest VExpr

[<Tests>]
let tests =
    testList "Basic literal tests" (List.map vExprTest dotAccessTests)