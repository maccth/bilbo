module Bilbo.Tests.ParserTests.Helpers

open Expecto
open Bilbo.Parser.Ast
open Bilbo.Parser.Parser
open FParsec

// Helper functions to make AST construction quicker
let Int x = x |> IntLit |> Literal
let Str x = x |> StrLit |> Literal
let Flt x = x |> FloatLit |> Literal
let Bool x = x |> BoolLit |> Literal
let VAR x = x |> Var |> VExpr
let BE x op y = (SExpr x, op, SExpr y) |> SBinExpr
let OI t pLst = (t,pLst) |> ObjInstan |> ObjExpr
let DOT e id = (e,id) |> DotAccess |> VExpr

let sExprAssignAst var rhs =
    (VAR var, SExpr rhs) |> AssignmentExpr |> ExprStatement |> Statement

let sExprAssignTest codeStr var rhs =
    let expAst = [sExprAssignAst var rhs]
    let ast = pBilboStr codeStr |> function
        | Success(res, _, _) -> res
        // TODO: Deal with failures by raising Expecto failure
        // | Failure(msg, err, state) -> failwith "Parsing failed"
    Expect.equal expAst ast ""

let createTest t =
    let code, var, rhs, des = t
    testCase des <| fun _ -> sExprAssignTest code var rhs