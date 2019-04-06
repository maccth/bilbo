module Bilbo.Tests.ParserTests.SimpleExpression

open Expecto
open Bilbo.Parser.Ast
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

let OIs t pLst = OI t (List.map SExpr pLst)
let objExprTests = [
    "a = MyType()", "a", OI "MyType" [], "Instantiation, no params";
    "a = MyType(p1,p2,p3)", "a", OI "MyType" (List.map VAR ["p1"; "p2"; "p3"]), "Instantiation, var params";
    "a = MyType'(\"One\",-2.0,3,False)", "a", OIs "MyType'" [STR "One"; FLT -2.0; INT 3; BOOL false], "Instantiation, literal params";
]

let prefixOpExprTests = [
    "a = &b", "a", PE PreOp.Amp (VAR "b"), "Get id var";
    "a = not b", "a", PE PreOp.Amp (VAR "b"), "Not var";
    "a = not True", "a", PE Not (true |> BOOL |> SExpr), "Not bool, true";
    "a = not False", "a", PE Not (false |> BOOL |> SExpr), "Not bool, false";
]

let numericBinExprTests = [
    "a=4+5", "a", BE (INT 4) Plus  (INT 5), "Int plus";
    "a=-4+-5", "a", BE (INT -4) Plus (INT -5), "Int plus, both negative";
    "a=-4+5", "a", BE (INT -4) Plus (INT 5), "Int plus, first negative";
    "a=4+-5", "a", BE (INT 4) Plus (INT -5), "Int plus, second negative";

    "a=1.23+4.56", "a", BE (FLT 1.23) Plus  (FLT 4.56), "Float plus";
    "a=-1.23+-4.56", "a", BE (FLT -1.23) Plus (FLT -4.56), "Float plus, both negative";
    "a=-1.23+4.56", "a", BE (FLT -1.23) Plus (FLT 4.56), "Float plus, first negative";
    "a=1.23+-4.56", "a", BE (FLT 1.23) Plus (FLT -4.56), "Float plus, second negative";

    "a=4-5", "a", BE (INT 4) Minus  (INT 5), "Int minus";
    "a=-4--5", "a", BE (INT -4) Minus (INT -5), "Int minus, both negative";
    "a=-4-5", "a", BE (INT -4) Minus (INT 5), "Int minus, first negative";
    "a=4--5", "a", BE (INT 4) Minus (INT -5), "Int minus, second negative";

    "a=1.23-4.56", "a", BE (FLT 1.23) Minus  (FLT 4.56), "Float minus";
    "a=-1.23--4.56", "a", BE (FLT -1.23) Minus (FLT -4.56), "Float minus, both negative";
    "a=-1.23-4.56", "a", BE (FLT -1.23) Minus (FLT 4.56), "Float minus, first negative";
    "a=1.23--4.56", "a", BE (FLT 1.23) Minus (FLT -4.56), "Float minus, second negative";
]

let sExprTest = consAssignTest SExpr
let exprTest = consAssignTest (fun x -> x)

[<Tests>]
let tests =
    testList "Basic literal tests" (List.map sExprTest literalTests)

[<Tests>]
let tests2 =
    testList "Binary expression tests" (List.map sExprTest objExprTests)

[<Tests>]
let tests3 =
    testList "Binary expression tests" (List.map exprTest numericBinExprTests)
   