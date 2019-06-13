module Bilbo.Tests.EvaluatorTests.Helpers

open Expecto
open Bilbo.Common.Value
open Bilbo.Common.Result
open Bilbo.Common.SymbolTable
open Bilbo.Parser.Parser
open Bilbo.Evaluator.Evaluator
open Bilbo.Graph.Graph

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

let prepNodes = Graph.nodes >> List.sort

let prepEdges =  Graph.edges >> List.sort

let prepGraph g = g |> Graph.nodes |> List.sort, g |> Graph.edges |> List.sort

let prepCollection c = c |> Set.toList |> List.map prepGraph |> List.sort 

let graphEquals (gotG : Graph) (expG : Graph) =
    (prepNodes gotG, prepEdges gotG) = (prepNodes expG, prepEdges expG)

let collectionEquals (gotC : Collection) (expC : Collection) =
    (prepCollection gotC) = (prepCollection expC)

let graphEqualsTest (gotG : Graph) (expG : Graph) =
    Expect.equal (prepNodes gotG, prepEdges gotG) (prepNodes expG, prepEdges expG) "Graph equality"

let collectionEqualsTest (gotC : Collection) (expC : Collection) =
    Expect.equal (prepCollection gotC) (prepCollection expC) "Collection equality"

let twinVarEqualsTest gotMean expMean =
    match gotMean, expMean with
    | Ok (Value(Graph g1)), Ok (Value(Graph(g2))) -> graphEqualsTest g1 g2
    | Ok (Value(Collection c1)), Ok (Value(Collection c2)) -> collectionEqualsTest c1 c2
    | Ok g, Ok m -> Expect.equal g m ""
    | Error e, _ 
    | _, Error e -> failwithf "Test failed. %A" e

let runTwinVarTest codeStr gotVar expVar =
    let gotSymsRes = codeStr |> bilboStringParser |> bilboEvaluator
    match gotSymsRes with
    | Error e -> failwithf "Test failed. %A" e
    | Ok syms ->
        let gotMean = Symbols.find syms {id=gotVar; spLst=[]}
        let expMean = Symbols.find syms {id=expVar; spLst=[]}
        twinVarEqualsTest gotMean expMean

let consTwinVarTest codeStr var1 var2 des =
    testCase des <| fun _ -> runTwinVarTest codeStr var1 var2

let twinVarTests testDataLst =
    let consTest testData =
        let codeStr, var1, var2, des = testData
        consTwinVarTest codeStr var1 var2 des
    testDataLst |> List.map consTest

let abTwinVarTests tLst =
    tLst
    |> List.map (fun (codeStr,des) -> (codeStr,"got","exp",des))
    |> twinVarTests

let runOneOfTest codeStr gotVar expVars =
    let gotSymsRes = codeStr |> bilboStringParser |> bilboEvaluator
    match gotSymsRes with
    | Error e -> failwithf "Test failed. %A" e
    | Ok gotSyms ->
        let got = Symbols.find gotSyms {id=gotVar; spLst=[]}
        let exps =
            let res var = Symbols.find gotSyms {id=var; spLst=[]}
            List.map res expVars
        let check exp =
            match got,exp with
            | Ok (Value(Graph g1)), Ok (Value(Graph(g2))) -> graphEquals g1 g2
            | Ok (Value(Collection c1)), Ok (Value(Collection c2)) -> collectionEquals c1 c2
            | Ok m1, Ok m2 -> m1 = m2
            | Error e, _ -> true
            | _, Error e -> true
        let ans = List.tryFind check exps
        match ans with
        | None ->
            List.map (fun e -> Expect.equal got e "") exps
            |> ignore
        | Some correctExp -> twinVarEqualsTest got correctExp

let consOneOfTest codeStr gotVar expVars des =
    testCase des <| fun _ -> runOneOfTest codeStr gotVar expVars

let aOneOfTests tLst =
    tLst
    |> List.map (fun (codeStr,expVars,des) -> consOneOfTest codeStr "got" expVars des)   

let nodes = """
na = "nodeA"::1001
nb = "nodeB"::1002
nc = "nodeC"::1003
nd = "nodeD"::1004
ne = "nodeE"::1005
nf = "nodeF"::1006
"""