module Bilbo.Evaluator.Evaluator

open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Common.Error
open Bilbo.Evaluator.ExpressionStatement

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
    | FunctionDefL(loc, fDef) ->
        let fName,fParams,fBod,fRet = fDef
        let fst = Symbols.head syms
        let syms' =
            match fParams with
            | [] ->
                let fMean = evalFuncBody syms rnLst fst fBod fRet
                match fMean with
                | Ok fRes -> Symbols.set syms {id=fName; spLst=rnLst} fRes
                | Error e -> e |> Error
            | _ ->
                let f = (fDef, fst) |> Function |> fun s -> [s] |> Pipeline |> Value
                Symbols.set syms {id=fName; spLst=rnLst} f
        attachLoc loc syms'
    | TransformDefL(loc, _) ->
        "Transforms"
        |> notImplementedYet
        |> attachLoc loc

let evalBilbo syms ast =
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
    evalRec syms ast    

let bilboEvaluatorSymsIn (ast : BilboResult<Program>) (symsIn : Symbols) : ProgramResult<Symbols> =
    match ast with
    | Ok ast' ->
        evalBilbo symsIn ast'
    | Error e ->
       e |> Error |> noLoc

let bilboEvaluator (ast : BilboResult<Program>) : ProgramResult<Symbols> =
    bilboEvaluatorSymsIn ast Symbols.empty

let bilboEvaluatorPrint (ast : BilboResult<Program>) =
    ast
    |> bilboEvaluator
    |> printfn "%A"