module Bilbo.Evaluator.Evaluator

open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Common

let evalProgramUnit symTabs pUnit =
    let nLst,s = pUnit
    match s with
    | TypeDefL (loc,def) ->
        let tname = fst def
        let symTabs' = SymbolTable.addType symTabs nLst tname def
        symTabs'
    | _ -> symTabs

let bilboEvaluate (ast : Program) =
    let rec evalRec symTabs ast  =
        match ast with
        | pUnit :: rest ->
           let symTabs' = evalProgramUnit symTabs pUnit
           evalRec symTabs' rest
        | [] ->
            symTabs
    evalRec [SymbolTable.empty] ast 

let bilboEvaluatorPrint (ast : Program) =
    ast
    |> bilboEvaluate
    |> printfn "%A"