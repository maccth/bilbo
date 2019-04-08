module Bilbo.SemanticAnalyser.SemanticAnalyser

open Bilbo.Parser.Ast
open Bilbo.Parser.Parser

let rec checkLhsAssign s =
    let rec checkLhsExpr lhs =
        match lhs with
        | Var _id -> true
        | BinExpr (l, Dot, r) ->
            (checkLhsExpr l) && (checkLhsExpr r)
        | _ -> false
    match s with
    | ExprStatement (AssignmentExpr(lhs,rhs)) -> checkLhsExpr lhs
    | _ -> false
            
// Just a code sketch...
let rec analyseSemanticsPrint (astIn : ProgramUnit list) astOut =
    match astIn with
    | Statement (nlst, line):: rest ->
        match checkLhsAssign line with
        | true -> analyseSemanticsPrint rest (Statement (nlst, line) :: astOut)
        | false -> printfn "Invalid assignment."
    | [] ->
        printfn "%A" (List.rev astOut)

    






