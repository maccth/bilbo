module Bilbo.Tests.ParserTests.SimpleExpression

open Expecto
open Bilbo.Common.Ast
open Bilbo.Tests.ParserTests.Helpers

let idTests = [
    "inta = 10", "inta", INT 10, "Identifier starts with int keyword, followed by letter"
    "return_ = 20", "return_", INT 20, "Identifier starts with return, followed by underscore"
    "import5 = 30", "import5", INT 30, "Identifier starts with import, followed by digit"
    "is' = 40", "is'", INT 40, "Identifier starts with is, followed by prime"
]

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
    testList "Identifier tests" (eExprTests idTests)

[<Tests>]
let tests2 =
    testList "Literal tests" (eExprTests literalTests)

[<Tests>]
let tests3 =
    testList "Object expression tests" (eExprTests objExprTests)

[<Tests>]
let tests4 =
    testList "Param list tests" (eExprTests paramListTests)