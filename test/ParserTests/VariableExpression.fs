module Bilbo.Tests.ParserTests.VariableExpression

open Expecto
open Bilbo.Parser.Ast
open Bilbo.Tests.ParserTests.Helpers

let objExprTests = [
    "a = MyType()", "a", OI "MyType" [], "Instantiation, no params";
    "a = MyType(p1,p2,p3)", "a", OI "MyType" (List.map VAR ["p1"; "p2"; "p3"]), "Instantiation, params";
    // "a = obj.b", "a", Dot (Var "obj") "b", "Dot access, one";
]

[<Tests>]
let tests =
    testList "Basic literal tests" (List.map createTest objExprTests)