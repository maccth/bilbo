module Bilbo.Tests.ParserTests.SimpleExpression

open Expecto
open Bilbo.Parser.Ast
open Bilbo.Parser.Parser
open FParsec

let sExprAssignAst var rhs =
    Statement(ExprStatement(AssignmentExpr(var, SExpr(rhs))))

// Helper functions to make AST construction quicker
let Int x = Literal(IntLit(x))
let Str x = Literal(StrLit(x))
let Flt x = Literal(FloatLit(x))
let Bool x = Literal(BoolLit(x))
let Var x = SExpr.SVar(x)
let BE x op y = SBinExpr(x, op, y) |> SBinExpr |> SExpr.BinExpr 

let sExprAssignTest codeStr var rhs =
    let expAst = [sExprAssignAst var rhs]
    let ast = pBilboStr codeStr |> function
        | Success(res, _, _) -> res
        // TODO: Deal with failures by raising Expecto failure
        // | Failure(msg, err, state) -> failwith "Parsing failed"
    Expect.equal expAst ast ""

let basicLiteralTests = [
    "Int", "a=10", "a", Int 10;
    "Negative int", "a=-6", "a", Int -6;
    "Float", "a=3.14159", "a", Flt 3.14159;
    "Negative float", "a=-0.14159", "a", Flt -0.14159;
    "String", "a=\"Hello, world!\"", "a", Str "Hello, world!";
    "Bool true", "a=True", "a", Bool true;
    "Bool false", "a=False", "a", Bool false;
]

let nodeConsTests = [
    "String identifier, int load", "a = \"NodeId\"::-10", "a", SExpr.NodeCons(Str "NodeId", Int -10);
    "Int identifier, bool load", "a = 0::True", "a", SExpr.NodeCons(Int 0, Bool true);
    "Var identifier, var load", "a = b::c", "a", SExpr.NodeCons(Var "b", Var "c");
]

let binExprTests = [
    "Int plus", "a=4+5", "a", BE (Int 4) SBinOp.Plus  (Int 5);
    "Int plus, both negative", "a=-4+-5", "a", BE (Int -4) SBinOp.Plus (Int -5);
    "Int plus, first negative", "a=-4+5", "a", BE (Int -4) SBinOp.Plus (Int 5);
    "Int plus, second negative", "a=4+-5", "a", BE (Int 4) SBinOp.Plus (Int -5);

    "Float plus", "a=1.23+4.56", "a", BE (Flt 1.23) SBinOp.Plus  (Flt 4.56);
    "Float plus, both negative", "a=-1.23+-4.56", "a", BE (Flt -1.23) SBinOp.Plus (Flt -4.56);
    "Float plus, first negative", "a=-1.23+4.56", "a", BE (Flt -1.23) SBinOp.Plus (Flt 4.56);
    "Float plus, second negative", "a=1.23+-4.56", "a", BE (Flt 1.23) SBinOp.Plus (Flt -4.56);
]

let createTest t =
    let des, code, var, rhs = t
    testCase des <| fun _ -> sExprAssignTest code var rhs

[<Tests>]
let tests =
    testList "Basic literal tests" (List.map createTest basicLiteralTests)

[<Tests>]
let tests2 =
    testList "Node construction tests" (List.map createTest nodeConsTests)

[<Tests>]
    let tests3 =
        testList "Binary expression tests" (List.map createTest binExprTests)