module Bilbo.Tests.ParserTests.SimpleExpression

open Expecto
open Bilbo.Parser.Ast
open Bilbo.Tests.ParserTests.Helpers

let basicLiteralTests = [
    "a=10", "a", Int 10, "Int";
    "a=-6", "a", Int -6, "Negative int";
    "a=3.14159", "a", Flt 3.14159, "Float";
    "a=-0.14159", "a", Flt -0.14159, "Negative float";
    "a=\"Hello, world!\"", "a", Str "Hello, world!", "String";
    "a=\"'Goodbye, world!'\"", "a", Str "'Goodbye, world!'", "String with single quotes";
    "a=True", "a", Bool true, "Bool true";
    "a=False", "a", Bool false, "Bool false";
]

let numericBinExprTests = [
    "a=4+5", "a", BE (Int 4) Plus  (Int 5), "Int plus";
    "a=-4+-5", "a", BE (Int -4) Plus (Int -5), "Int plus, both negative";
    "a=-4+5", "a", BE (Int -4) Plus (Int 5), "Int plus, first negative";
    "a=4+-5", "a", BE (Int 4) Plus (Int -5), "Int plus, second negative";

    "a=1.23+4.56", "a", BE (Flt 1.23) Plus  (Flt 4.56), "Float plus";
    "a=-1.23+-4.56", "a", BE (Flt -1.23) Plus (Flt -4.56), "Float plus, both negative";
    "a=-1.23+4.56", "a", BE (Flt -1.23) Plus (Flt 4.56), "Float plus, first negative";
    "a=1.23+-4.56", "a", BE (Flt 1.23) Plus (Flt -4.56), "Float plus, second negative";

    "a=4-5", "a", BE (Int 4) Minus  (Int 5), "Int minus";
    "a=-4--5", "a", BE (Int -4) Minus (Int -5), "Int minus, both negative";
    "a=-4-5", "a", BE (Int -4) Minus (Int 5), "Int minus, first negative";
    "a=4--5", "a", BE (Int 4) Minus (Int -5), "Int minus, second negative";

    "a=1.23-4.56", "a", BE (Flt 1.23) Minus  (Flt 4.56), "Float minus";
    "a=-1.23--4.56", "a", BE (Flt -1.23) Minus (Flt -4.56), "Float minus, both negative";
    "a=-1.23-4.56", "a", BE (Flt -1.23) Minus (Flt 4.56), "Float minus, first negative";
    "a=1.23--4.56", "a", BE (Flt 1.23) Minus (Flt -4.56), "Float minus, second negative";
]

[<Tests>]
let tests =
    testList "Basic literal tests" (List.map createTest basicLiteralTests)

[<Tests>]
let tests2 =
    testList "Binary expression tests" (List.map createTest numericBinExprTests)