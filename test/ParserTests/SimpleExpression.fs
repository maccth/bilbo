module Bilbo.Tests.ParserTests.SimpleExpression

open Expecto
open Bilbo.Common.Ast
open Bilbo.Tests.ParserTests.Helpers

let literalTests = [
    "a=10", "a", INT 10, "Int";
    "a=-6", "a", INT -6, "Negative int";
    "a=3.14159", "a", FLT 3.14159, "Float";
    "a=-0.14159", "a", FLT -0.14159, "Negative float";
    "a=\"Hello, world!\"", "a", STR "Hello, world!", "String";
    "a=\"'Goodbye, world!'\"", "a", STR "'Goodbye, world!'", "String with single quotes";
    "a=True", "a", BOOL true, "Bool true";
    "a=False", "a", BOOL false, "Bool false";
]

let OBJs t pLst = OBJ t (List.map SExpr pLst)
let objExprTests = [
    "a = MyType()", "a", OBJs "MyType" [], "Instantiation, no params";
    "a = MyType(p1,p2,p3)", "a", OBJ "MyType" (List.map VAR ["p1"; "p2"; "p3"]), "Instantiation, var params";
    "a = MyType'(\"One\",-2.0,3,False)", "a", OBJs "MyType'" [STR "One"; FLT -2.0; INT 3; BOOL false], "Instantiation, literal params";
]

let INTe x = x |> INT |> SExpr
// Some of these tests are not valid semantically since param lists cannot be bound
let paramListTests = [
    "a = (1,2,3)", "a", [1;2;3] |> List.map INTe |> PLST, "Param list, 3 ints";
    "a = (1,2)", "a", [1;2] |> List.map INTe |> PLST , "Param list, 2 ints";
    "a = (1)", "a", INT 1, "Single values in brackets are not param lists";
    "a = (1, 2.0, \"Three\", False)",
        "a",
        [INT 1; FLT 2.0; STR "Three"; BOOL false] |> List.map SExpr |> PLST,
        "Param list, different types of literals";
]

let eExprTests = List.map sExprTest

[<Tests>]
let tests =
    testList "Literal tests" (eExprTests literalTests)

[<Tests>]
let tests2 =
    testList "Object expression tests" (eExprTests objExprTests)

[<Tests>]
let tests3 =
    testList "Param list tests" (eExprTests paramListTests)