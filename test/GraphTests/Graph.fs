module Bilbo.Tests.GraphTests.Graph

open Expecto
open Bilbo.Common.Extensions
open Bilbo.Graph.Graph
open Bilbo.Tests.GraphTests.Helpers

[<Tests>]
let test1 =
    // [A,>,B]
    let g =
        Graph.empty
        |> Graph.addNode A
        |> Graph.addEdge (A .>. B)
    let expG =
        Graph.empty
        |> Graph.addNodes [A;B]
    let gotG = Graph.subtractGraphs g g 
    testCase "Bilbo graph subtraction. G-G = edge endpoints of G" <| fun _ ->
        Expect.equal gotG expG ""

[<Tests>]
let test2 =
    let g =
        Graph.empty
        |> Graph.addEdge (A .>. B)
        |> Graph.addEdge (A .>. D)
        |> Graph.addNodes [C;D;E]
    let expG =
        Graph.empty
        |> Graph.addNodes [A;B;D]
    let gotG = Graph.subtractGraphs g g
    testCase "Bilbo graph subtraction II" <| fun _ -> Expect.equal gotG expG ""