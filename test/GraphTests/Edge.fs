module Bilbo.Tests.GraphTests.Edge

open Expecto
open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.Error
open Bilbo.Common.SymbolTable
open Bilbo.Graph.Graph
open Bilbo.Common

let bilboStr = String >> Value
let bilboInt = Int >> Value
let bilbo1 = 1 |> bilboInt
let nd id = {id=id|>bilboStr; load=bilbo1}

let a = nd "a"
let b = nd "b"
let c = nd "c"
let d = nd "d"

let eLst = [
    (a, Some 40,    b)
    (a, Some 200,   b)
    (b, None,       d)
    (b, Some 15,    c)
    (c, Some 30,    a)
    (d, None,       a)
]

let gExp =
    {
        nodes = ["a"; "b"; "c"; "d"] |> List.map (fun id -> (id |> bilboStr, bilbo1)) |> Map.ofList
        sourceEdges =  Map.ofList [
            bilboStr "a",
                Map.ofList [bilboStr "b", [200 |> bilboInt |> Some; 40 |> bilboInt |> Some]];
            bilboStr "b",
                Map.ofList [bilboStr "d", [None]; bilboStr "c", [15 |> bilboInt |> Some]];
            bilboStr "c",
                Map.ofList [bilboStr "a", [30 |> bilboInt |> Some]];                    
            bilboStr "d",
                Map.ofList [bilboStr "a",[None]];
        ];
        targetEdges = Map.ofList [
            bilboStr "b",
                Map.ofList [bilboStr "a", [200 |> bilboInt |> Some; 40 |> bilboInt |> Some]];
            bilboStr "d",
                Map.ofList [bilboStr "b", [None]];
            bilboStr "c",
                Map.ofList [bilboStr "b", [15 |> bilboInt |> Some]];        
            bilboStr "a",
                Map.ofList [bilboStr "d", [None]; bilboStr "c", [30 |> bilboInt |> Some]];               
        ] 
    }

let consStringIntELst lst =
    let stringIntE (s,w,t) =
        match w with
        | None -> {source=s; weight=None; target=t}
        | Some i ->
            i
            |> bilboInt
            |> Some
            |> fun w' -> {source=s; weight=w'; target=t}
    List.map stringIntE lst        

let g = Graph.empty
let gGot = Graph.addEdges g (eLst |> consStringIntELst)


[<Tests>]
let test =
    testCase "Adding edges to graph test" <| fun _ ->
        match gGot with
        | Ok gGot' -> Expect.equal gGot' gExp ""
        | Error e -> failwithf "Test errored. %A" e