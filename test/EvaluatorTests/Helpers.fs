module Bilbo.Tests.EvaluatorTests.Helpers

open Expecto
open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.Error
open Bilbo.Parser.Parser
open Bilbo.Evaluator.Evaluator
open Bilbo.Common.SymbolTable

let oneVarSyms id mean : Symbols =
    let st = [id, mean] |> Map.ofList
    [st]

let pickImportantVar syms var =
    let vid = {id=var; spLst=[]}
    let important = Symbols.find syms vid
    // If the important var doesn't exist in the syms, forward original syms
    // since this will also fail the expecto check
    match important with
    | Error _ -> syms
    | Ok important' ->
        let syms' = Symbols.set Symbols.empty vid important'
        match syms' with
        | Ok importantSyms -> importantSyms
        | Error _ -> syms

let runSingleVarTest codeStr var mean =
    let expSyms = oneVarSyms var mean
    let gotSymsRes = codeStr |> bilboStringParser |> bilboEvaluator
    match gotSymsRes with
    | Ok gotSyms ->
        let importantSyms = pickImportantVar gotSyms var
        Expect.equal importantSyms expSyms ""
    | Error e ->
        failwithf "Test failed. %A" e

let consSingleVarTest codeStr var mean des =
    testCase des <| fun _ -> runSingleVarTest codeStr var mean

let singleVarTest testData =
    let codeStr, var, mean, des = testData
    consSingleVarTest codeStr var mean des

let singleVarTests testDataLst =
    testDataLst |> List.map singleVarTest
