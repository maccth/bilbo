module Bilbo.Evaluator.Evaluator

open Bilbo.Common.Extensions
open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Common.Result
open Bilbo.Evaluator.ExpressionStatement

let attachLoc loc res =
    match res with
    | Error e ->
        match e with
        | {loc=None} -> {e with loc=Some loc} |> Error
        | _ -> e |> Error
    | Ok r -> r |> Ok

let nLstStr nLst id =
    let folder s space =
        match space with
        | Name n -> s + n + "." 
        | NodePart _ ->
            "NodePart SpaceIds should not be present in the AST before evaluation-time"
            |> implementationError
            |> failwithf "%A"
    List.fold folder "" nLst + id 

let evalProgramUnit (syms : Symbols) pUnit : BilboResult<Symbols> =
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
        |-/> BilboError.addExtra ("Within type definition of " + tName + ".")
    | ExprStatementL (loc, e) ->
        evalExprStatement syms rnLst e
        |> attachLoc loc
    | ImportL (loc, im) ->
        let syms',fp =
            match im with
            | ImportAs (fp,alias) ->
                Symbols.set syms {spLst=nLst ; id=alias} ((Namespace, SymbolTable.empty) |> Space), fp+"("+alias+")"
            | ImportTo (fp) -> syms |> Ok, fp       
        attachLoc loc syms'
        |-/> BilboError.addExtra ("Within import of " + fp + ".")
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
                let f = (fDef, fst) |> Function |> PStage |> Pipeline |> Value
                Symbols.set syms {id=fName; spLst=rnLst} f
        attachLoc loc syms'
        |-/> BilboError.addExtra ("Within function definition of " + nLstStr rnLst fName + ".")
    | TransformDefL(loc, tDef) ->
        let tName,_,_,_ = tDef
        let tst = Symbols.head syms
        let t = (tDef, tst) |> Transform |> PStage |> Pipeline |> Value
        let syms' = Symbols.set syms {id=tName; spLst=rnLst} t
        attachLoc loc syms'
        |-/> BilboError.addExtra ("Within transform definition of " + nLstStr rnLst tName + ".")

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

let bilboEvaluatorSymsIn (ast : BilboResult<Program>) (symsIn : Symbols) : BilboResult<Symbols> =
    match ast with
    | Ok ast' -> evalBilbo symsIn ast'
    | Error e -> e |> Error

let bilboEvaluator (ast : BilboResult<Program>) : BilboResult<Symbols> =
    bilboEvaluatorSymsIn ast Symbols.empty

let bilboEvaluatorPrint (ast : BilboResult<Program>) =
    ast
    |> bilboEvaluator
    |> printfn "%A"