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

let gExp : Graph =
    {
        nodes = ["a"; "b"; "c"; "d"] |> List.map (fun id -> (id |> bStr, b1)) |> Map.ofList
        edges = Map.ofList [
                (0, {Edge.source=a; weight=Some (bInt 40); target=b})
                (1, {Edge.source=a; weight=Some (bInt 200); target=b})
                (2, {Edge.source=b; weight=Some (bInt 15); target=c})
                (3, {Edge.source=b; weight=None; target=d})
                (4, {Edge.source=c; weight=Some (bInt 30); target=a})
                (5, {Edge.source=d; weight=None; target=a})
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