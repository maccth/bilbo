module Bilbo.Tests.GraphTests.Edge

open Expecto
open Bilbo.Common.Value
open Bilbo.Graph.Graph
open Bilbo.Tests.GraphTests.Helpers

let a = nd1 "a"
let b = nd1 "b"
let c = nd1 "c"
let d = nd1 "d"

let eLst = [
    (a, Some 40,    b) // 0
    (a, Some 200,   b) // 1
    (b, Some 15,    c) // 2
    (b, None,       d) // 3
    (c, Some 30,    a) // 4
    (d, None,       a) // 5
]

let gExp =
    {
        nodes = ["a"; "b"; "c"; "d"] |> List.map (fun id -> (id |> bilboStr, b1)) |> Map.ofList
        sourceEdges =  Map.ofList [
            bilboStr "a",
                Map.ofList [bilboStr "b", [(200 |> bInt |> Some),1; (40 |> bInt |> Some),0]];
            bilboStr "b",
                Map.ofList [bilboStr "d", [None,3]; bilboStr "c", [15 |> bInt |> Some,2]];
            bilboStr "c",
                Map.ofList [bilboStr "a", [(30 |> bInt |> Some),4]];                    
            bilboStr "d",
                Map.ofList [bilboStr "a",[None,5]];
        ];
        targetEdges = Map.ofList [
            bilboStr "b",
                Map.ofList [bilboStr "a", [200 |> bInt |> Some,1; 40 |> bInt |> Some,0]];
            bilboStr "d",
                Map.ofList [bilboStr "b", [None,3]];
            bilboStr "c",
                Map.ofList [bilboStr "b", [15 |> bInt |> Some,2]];        
            bilboStr "a",
                Map.ofList [bilboStr "d", [None,5]; bilboStr "c", [30 |> bInt |> Some,4]];               
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
    testCase "Adding edges to graph test" <| fun _ ->
        match gGot with
        | Ok gGot' -> Expect.equal gGot' gExp ""
        | Error e -> failwithf "Test errored. %A" e