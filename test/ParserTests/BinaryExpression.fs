module Bilbo.Tests.ParserTests.BinaryExpression

open Expecto
open Bilbo.Common.Ast
open Bilbo.Tests.ParserTests.Helpers

let numericBinExprTests = [
    "a=4+5", "a", BIN (INT 4) Plus  (INT 5), "Int plus";
    "a=-4+-5", "a", BIN (INT -4) Plus (INT -5), "Int plus, both negative";
    "a=-4+5", "a", BIN (INT -4) Plus (INT 5), "Int plus, first negative";
    "a=4+-5", "a", BIN (INT 4) Plus (INT -5), "Int plus, second negative";

    "a=1.23+4.56", "a", BIN (FLT 1.23) Plus  (FLT 4.56), "Float plus";
    "a=-1.23+-4.56", "a", BIN (FLT -1.23) Plus (FLT -4.56), "Float plus, both negative";
    "a=-1.23+4.56", "a", BIN (FLT -1.23) Plus (FLT 4.56), "Float plus, first negative";
    "a=1.23+-4.56", "a", BIN (FLT 1.23) Plus (FLT -4.56), "Float plus, second negative";

    "a=4-5", "a", BIN (INT 4) Minus  (INT 5), "Int minus";
    "a=-4--5", "a", BIN (INT -4) Minus (INT -5), "Int minus, both negative";
    "a=-4-5", "a", BIN (INT -4) Minus (INT 5), "Int minus, first negative";
    "a=4--5", "a", BIN (INT 4) Minus (INT -5), "Int minus, second negative";

    "a=1.23-4.56", "a", BIN (FLT 1.23) Minus  (FLT 4.56), "Float minus";
    "a=-1.23--4.56", "a", BIN (FLT -1.23) Minus (FLT -4.56), "Float minus, both negative";
    "a=-1.23-4.56", "a", BIN (FLT -1.23) Minus (FLT 4.56), "Float minus, first negative";
    "a=1.23--4.56", "a", BIN (FLT 1.23) Minus (FLT -4.56), "Float minus, second negative";
]

let nodeConsTests = [
    "String identifier, int load", "a = \"NodeId\"::-10", "a", BIN (STR  "NodeId") Dot (INT -10);
    "Int identifier, bool load", "a = 0::True", "a", BIN (INT 0) NodeCons (BOOL true);
    "Var identifier, var load", "a = b::c", "a", (VAR "b", NodeCons, VAR "c") |> BinExpr;
]

[<Tests>]
let tests3 =
    testList "Binary expression tests" (List.map exprTest numericBinExprTests)