module Bilbo.Tests.EvaluatorTests.Helpers

open Expecto
open Bilbo.Common.Value
open Bilbo.Parser.Parser
open Bilbo.Evaluator.Evaluator
open Bilbo.Graph.Graph
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

let graphEquals (gotG : Graph) (expG : Graph) =
    let nodes = Graph.nodes >> List.sort
    let edges = Graph.edges >> List.sort
    Expect.equal (nodes gotG, edges gotG) (nodes expG, edges expG) "Graph equality"

let collectionEquals (gotC : Collection) (expC : Collection) =
    let prepareG g = g |> Graph.nodes |> List.sort, g |> Graph.edges |> List.sort
    let prepareC c = c |> Set.toList |> List.map prepareG |> List.sort
    Expect.equal (prepareC gotC) (prepareC expC) "Collection equality"

let runTwinVarTest codeStr var1 var2 =
    let gotSymsRes = codeStr |> bilboStringParser |> bilboEvaluator
    match gotSymsRes with
    | Error e -> failwithf "Test failed. %A" e
    | Ok gotSyms ->
        let mean1Res =  Symbols.find gotSyms {id=var1; spLst=[]}
        let mean2Res = Symbols.find gotSyms {id=var2; spLst=[]}
        match mean1Res, mean2Res with
        | Ok (Value(Graph g1)), Ok (Value(Graph(g2))) -> graphEquals g1 g2
        | Ok (Value(Collection c1)), Ok (Value(Collection c2)) -> collectionEquals c1 c2
        | Ok m1, Ok m2 -> Expect.equal m1 m2 ""
        | Error e, _ 
        | _, Error e -> failwithf "Test failed. %A" e
        
let consTwinVarTest codeStr var1 var2 des =
    testCase des <| fun _ -> runTwinVarTest codeStr var1 var2

let twinVarTest testData =
    let codeStr, var1, var2, des = testData
    consTwinVarTest codeStr var1 var2 des

let twinVarTests testDataLst =
    testDataLst |> List.map twinVarTest

let abTwinVarTest codeStr des =
    (codeStr, "a", "b", des) 

let abTwinVarTests tLst =
    tLst
    |> List.map (fun (c,d) -> abTwinVarTest c d)
    |> twinVarTests

let nodes = """
na = "nodeA"::1001
nb = "nodeB"::1002
nc = "nodeC"::1003
nd = "nodeD"::1004
ne = "nodeE"::1005
nf = "nodeF"::1006
"""