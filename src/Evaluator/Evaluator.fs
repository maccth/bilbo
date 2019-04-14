module Bilbo.Evaluator.Evaluator

open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Common.Error

let evalLiteral l : BilboResult<SymbolTable.Meaning> =
    match l with
    | StrLit s -> s |> String
    | FloatLit f -> f |> Float
    | IntLit i -> i |> Int
    | BoolLit b -> b |> Bool
    |> SymbolTable.Value
    |> Ok

let evalExpr symTabs nLst e : BilboResult<SymbolTable.Meaning> =
    match e with
    | Var v -> SymbolTable.getSymbolValue symTabs {nLst=nLst; oLst=[]; id=v}
    | SExpr(Literal l) -> evalLiteral l
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error

let consVid symTabs nLst e : BilboResult<ValueId> =
    match e with
    |Var v -> {nLst=nLst; oLst=[]; id=v} |> Ok
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error
    
let evalExprStatement symTabs nLst (e : ExprStatement) : BilboResult<ProgramSymbols> =
    match e with
    | AssignmentExpr (eLhs, eRhs) ->
        let lhsId = consVid symTabs nLst eLhs
        match lhsId with
        | Error e -> e |> Error
        | Ok vid ->
            let rhsVal = evalExpr symTabs nLst eRhs
            match rhsVal with
            | Error e -> e |> Error
            | Ok rhs ->
                SymbolTable.setSymbolValue symTabs vid rhs
                |> Ok
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error
                        

let evalProgramUnit symTabs pUnit : BilboResult<ProgramSymbols> =
    let nLst,s = pUnit
    let rnLst = List.rev nLst
    match s with
    | TypeDefL (loc,def) ->
        let tname = fst def
        let symTabs' = SymbolTable.addType symTabs rnLst tname def
        symTabs'
        |> Ok
    | ExprStatementL (loc, e) ->
        evalExprStatement symTabs rnLst e
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error

let bilboEvaluate (ast : BilboResult<Program>) : BilboResult<ProgramSymbols> =
    let rec evalRec symTabs ast  =
        match ast with
        | pUnit :: rest ->
           let symTabs' = evalProgramUnit symTabs pUnit
           match symTabs' with
           | Ok symTabs'' ->
                evalRec symTabs'' rest
            | Error e ->
                e |> Error
        | [] ->
            symTabs |> Ok
    match ast with
    | Ok ast' ->
        evalRec [SymbolTable.empty] ast'
    | Error err ->
       err |> Error

let bilboEvaluatorPrint (ast : BilboResult<Program>) =
    ast
    |> bilboEvaluate
    |> printfn "%A"