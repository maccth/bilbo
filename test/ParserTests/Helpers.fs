module Bilbo.Tests.ParserTests.Helpers

open Expecto
open Bilbo.Parser.Ast
open Bilbo.Parser.Parser
open FParsec

// Helper functions to make AST construction quicker
let VAR x = x |> Var |> VExpr

let INT x = x |> IntLit |> Literal  
let STR x = x |> StrLit |> Literal 
let FLT x = x |> FloatLit |> Literal 
let BOOL x = x |> BoolLit |> Literal 

let BE x op y = (SExpr x, op, SExpr y) |> BinExpr
let PE op x = (op, x) |> PrefixExpr
let OI t pLst = (t,pLst) |> ObjInstan |> ObjExpr
let DOT e id = (e,id) |> DotAssign

let runAstTest expAst codeStr =
    let ast = pBilboStr codeStr |> function
        | Success(res, _, _) -> res
        // TODO: Deal with failures by raising Expecto failure
        // | Failure(msg, err, state) -> failwith "Parsing failed"
    Expect.equal expAst ast ""
    
let consAssignAst var rhs =
    (VAR var, rhs) |> AssignmentExpr |> ExprStatement |> Statement

let runAssignTest codeStr var rhs =
    let expAst = [consAssignAst var rhs]
    runAstTest expAst codeStr
 
let consAssignTest eTyp data  =
    let code, var, rhs, des = data
    testCase des <| fun _ -> runAssignTest code var (eTyp rhs)