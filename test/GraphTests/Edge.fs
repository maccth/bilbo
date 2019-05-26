module Bilbo.Tests.GraphTests.Edge

open Expecto
open Bilbo.Common.Value
open Bilbo.Graph.Graph
open Bilbo.Tests.GraphTests.Helpers

let eLst = [
    (a, Some 40,    b) // 0
    (a, Some 200,   b) // 1
    (b, Some 15,    c) // 2
    (b, None,       d) // 3
    (c, Some 30,    a) // 4
    (d, None,       a) // 5
]


let gExp : Graph =
    {
        nodes = ["a",10; "b",20; "c",30; "d",40] |> List.map (fun (id,ld) -> (id |> bStr, ld |> bInt)) |> Map.ofList
        edges = Map.ofList [
                (0, {EdgeInfo.source=bStr "a"; weight=Some (bInt 40);   target=bStr "b"; id=0})
                (1, {EdgeInfo.source=bStr "a"; weight=Some (bInt 200);  target=bStr "b"; id=1})
                (2, {EdgeInfo.source=bStr "b"; weight=Some (bInt 15);   target=bStr "c"; id=2})
                (3, {EdgeInfo.source=bStr "b"; weight=None;             target=bStr "d"; id=3})
                (4, {EdgeInfo.source=bStr "c"; weight=Some (bInt 30);   target=bStr "a"; id=4})
                (5, {EdgeInfo.source=bStr "d"; weight=None;             target=bStr "a"; id=5})
            ]
        edgeIdCount = 6
    }

let consStringIntELst lst =
    let stringIntE (s,w,t) =
        match w with
        | None -> {Edge.source=s; weight=None; target=t}
        | Some i ->
            i
            |> bInt
            |> Some
            |> fun w' -> {Edge.source=s; weight=w'; target=t}
    List.map stringIntE lst        

let g = Graph.empty
let gGot = Graph.addEdges (eLst |> consStringIntELst) g

[<Tests>]
let test =
    testCase "Adding edges to graph test" <| fun _ -> Expect.equal gGot gExp ""