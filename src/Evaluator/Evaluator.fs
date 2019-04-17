module Bilbo.Evaluator.Evaluator

open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Common.Error
open Bilbo.Evaluator.ExpressionStatement

let evalProgramUnit (syms : Symbols) pUnit : BilboResult<Symbols> =
    let nLst,s = pUnit
    let rnLst = List.rev nLst
    match s with
    | TypeDefL (loc,def) ->
        let tname = fst def
        let vid = {spLst=rnLst; id=tname}
        let value = def |> Type |> Value
        Symbols.set syms vid value
    | ExprStatementL (loc, e) ->
        evalExprStatement syms rnLst e
    | ImportL (loc, i) ->
        syms |> Ok    
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error

let bilboEvaluate (ast : BilboResult<Program>) : BilboResult<Symbols> =
    let rec evalRec syms ast  =
        match ast with
        | pUnit :: rest ->
           let syms' = evalProgramUnit syms pUnit
           match syms' with
           | Ok syms'' ->
                evalRec syms'' rest
            | Error e ->
                e |> Error
        | [] ->
            syms |> Ok
    match ast with
    | Ok ast' ->
        evalRec Symbols.empty ast'
    | Error err ->
       err |> Error

let bilboEvaluatorPrint (ast : BilboResult<Program>) =
    ast
    |> bilboEvaluate
    |> printfn "%A"