module Bilbo.Evaluator.Evaluator

open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Common.Error
open Bilbo.Evaluator.ExpressionStatement
open Bilbo.Evaluator.Function

let attachLoc loc res : ProgramResult<'T> = Result.mapError (fun e -> (Some loc, e)) res
let noLoc res : ProgramResult<'T> = Result.mapError (fun e -> (None, e)) res

let nLstStr nLst id =
    let folder s space =
        match space with
        | Top -> s
        | Name n -> s + n + "." 
    List.fold folder "" nLst + id 

let evalProgramUnit (syms : Symbols) pUnit : ProgramResult<Symbols> =
    let nLst,s = pUnit
    let rnLst = List.rev nLst
    match s with
    | TypeDefL (loc,def) ->
        let idName = fst def
        let tName = nLstStr rnLst idName
        let vid = {spLst=rnLst; id=idName}
        let value = (tName, snd def) |> Type |> Value
        Symbols.set syms vid value
        |> attachLoc loc
    | ExprStatementL (loc, e) ->
        evalExprStatement syms rnLst e
        |> attachLoc loc
    | ImportL (loc, (fp, alias)) ->
        let syms' = Symbols.set syms {spLst=nLst ; id=alias;} ((Namespace, SymbolTable.empty) |> Space)
        attachLoc loc syms'
    | FunctionDefL(loc, fd) ->
        let syms' = evalfunctionDef syms rnLst fd
        attachLoc loc syms'
    | TransformDefL(loc, _) ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error
        |> attachLoc loc


let bilboEvaluator (ast : BilboResult<Program>) : ProgramResult<Symbols> =
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
       err |> Error |> noLoc

let bilboEvaluatorPrint (ast : BilboResult<Program>) =
    ast
    |> bilboEvaluator
    |> printfn "%A"