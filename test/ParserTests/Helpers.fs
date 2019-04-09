module Bilbo.Tests.ParserTests.Helpers

open Expecto
open Bilbo.Parser.Ast
open Bilbo.Parser.Parser
open FParsec

// Helper functions to make AST construction quicker
let VAR x = x |> Var

let INT x = x |> IntLit |> Literal  
let STR x = x |> StrLit |> Literal 
let FLT x = x |> FloatLit |> Literal 
let BOOL x = x |> BoolLit |> Literal 

let BIN x op y = (SExpr x, op, SExpr y) |> BinExpr
let PRE op x = (op, x) |> PrefixExpr
let POST op x = (x, op) |> PostfixExpr
let OBJ t pLst = (t,pLst) |> ObjInstan |> ObjExpr
let DOT e id = (e,Dot,id) |> BinExpr
let PLST lst = lst |> ParamList

let ND n = n |> Node
let REDGE n1 e n2 = (n1, Right e,n2) |> Edge
let LEDGE n1 e n2 = (n1,Left e,n2) |> Edge
let BIEDGE n1 e n2 = (n1,Bidir e,n2) |> Edge
let PATH lst = lst |> Path |> PathExpr |> GExpr

let runAstTest expAst codeStr =
    let ast = bilboStringParser codeStr
    Expect.equal expAst ast ""
    
let consAssignAst var rhs =
    (VAR var, rhs) |> AssignmentExpr |> ExprStatement |> fun s -> ([Top],s) |> ProgramUnit

let runAssignTest codeStr var rhs =
    let expAst = [consAssignAst var rhs]
    runAstTest expAst codeStr
 
let consAssignTest eTyp data  =
    let code, var, rhs, des = data
    testCase des <| fun _ -> runAssignTest code var (eTyp rhs)

let sExprTest = consAssignTest SExpr

let exprTest = consAssignTest id
let exprTests = List.map exprTest