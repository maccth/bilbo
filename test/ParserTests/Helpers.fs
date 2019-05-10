module Bilbo.Tests.ParserTests.Helpers

open Expecto
open Bilbo.Common.Ast
open Bilbo.Parser.Parser

// Helper functions to make AST construction quicker
let VAR x = x |> Var

let INT x = x |> IntLit |> Literal  
let STR x = x |> StrLit |> Literal 
let FLT x = x |> FloatLit |> Literal 
let BOOL x = x |> BoolLit |> Literal 

let BIN x op y = (SExpr x, op, SExpr y) |> BinExpr
let PRE op x = (op, x) |> PrefixExpr
let POST op x = (x, op) |> PostfixExpr
let OBJ t pLst = (t,pLst) |> ObjExpr
let DOT e id = (e,Dot,id) |> BinExpr
let PLST lst = lst |> ParamList

let ND n = n |> Node
let REDGE n1 e n2 = (n1, Right e,n2) |> Edge
let LEDGE n1 e n2 = (n1,Left e,n2) |> Edge
let BIEDGE n1 e n2 = (n1,Bidir e,n2) |> Edge
let PATH lst = lst |> Path |> PathExpr |> GExpr

let emptyLoc = {
        file = "test";
        startLine = -1L;
        startCol = -1L;
        endLine = -1L;
        endCol = -1L;
}

let overrideAstLocs (ast : ProgramUnit list) (loc : Loc) =
    let rec setAstLocs (astIn : ProgramUnit list) astOut =
        match astIn with
        | (nlst, s) :: rest ->
            let s' =
                match s with
                | TypeDefL (l,tdef) -> TypeDefL (loc, tdef)
                | TransformDefL (l,tdef) -> TransformDefL (loc, tdef)
                | FunctionDefL(l,fdef) -> FunctionDefL(loc, fdef)
                | ExprStatementL (l, e) -> ExprStatementL(loc, e)
                | ImportL (l, im) -> ImportL (loc, im)
            let altered = (nlst, s') |> ProgramUnit
            setAstLocs rest ((ProgramUnit altered):: astOut)
        | [] -> astOut
    setAstLocs ast []

let runAstTest expAst codeStr =
    let ast = bilboStringParser codeStr
    match ast with
    | Ok ast' ->
        let astTest = overrideAstLocs ast' emptyLoc
        let expAst = overrideAstLocs ast' emptyLoc
        Expect.equal expAst astTest ""
    | Error e ->
        failwithf "Test failed. %A" e

// TODO: Add failure tests
(*
let runAstFailTest errorFunc codeStr =
let ast = bilboStringParser codeStr
match ast with
| Error e ->
    let e' = errorFunc e
    Expect.isTrue e'
| Ok ast' ->
    failwithf "%s" "Meant to fail."
*)

let consAssignAst var rhs =
    (VAR var, rhs)
    |> AssignmentExpr
    |> fun e -> (emptyLoc, e)
    |> ExprStatementL
    |> fun s -> ([Top],s) |> ProgramUnit

let runAssignTest codeStr var rhs =
    let expAst = [consAssignAst var rhs]
    runAstTest expAst codeStr

let consAssignTest eTyp testData =
    let code, var, rhs, des = testData
    testCase des <| fun _ -> runAssignTest code var (eTyp rhs)

let sExprTest = consAssignTest SExpr

let exprTest = consAssignTest id
let exprTests = List.map exprTest