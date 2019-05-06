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

let runSingleVarTest codeStr var mean =
    let expSyms = oneVarSyms var mean
    let symsTestRes = codeStr |> bilboStringParser |> bilboEvaluator
    match symsTestRes with
    | Ok symsTest ->
        Expect.equal symsTest expSyms ""
    | Error e ->
        failwithf "Test failed. %A" e

let consSingleVarTest codeStr var mean des =
    testCase des <| fun _ -> runSingleVarTest codeStr var mean

let singleVarTest testData =
    let codeStr, var, mean, des = testData
    consSingleVarTest codeStr var mean des

let singleVarTests testDataLst =
    testDataLst |> List.map singleVarTest