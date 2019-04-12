module Bilbo.Evaluator.Evaluator

open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Common.Error

let evalProgramUnit symTabs pUnit =
    let nLst,s = pUnit
    match s with
    | TypeDefL (loc,def) ->
        let tname = fst def
        let symTabs' = SymbolTable.addType symTabs nLst tname def
        symTabs'
    | _ -> symTabs

let bilboEvaluate (ast : BilboResult<Program>) : BilboResult<ProgramSymbols> =
    let rec evalRec symTabs ast  =
        match ast with
        | pUnit :: rest ->
           let symTabs' = evalProgramUnit symTabs pUnit
           evalRec symTabs' rest
        | [] ->
            symTabs
    match ast with
    | Ok ast' ->
        evalRec [SymbolTable.empty] ast' |> Ok
    | Error err ->
       err |> Error

let bilboEvaluatorPrint (ast : BilboResult<Program>) =
    ast
    |> bilboEvaluate
    |> printfn "%A"