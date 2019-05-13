module Bilbo.Tests.GraphTests.Isomorphism

open Expecto
open Bilbo.Common.Value
open Bilbo.Graph.Graph
open Bilbo.Graph.Isomorphism
open Bilbo.Tests.GraphTests.Helpers
open Bilbo.Common.Error

let A = ndSi "A" 1
let B = ndSi "B" 2
let C = ndSi "C" 3
let D = ndSi "D" 4

let a = ndSi "a" 10
let b = ndSi "b" 20
let c = ndSi "c" 30
let d = ndSi "d" 40


// ["A"::1, >, "B"::2, >, "C"::3, "D":4]
let mainG =
    Graph.empty
    |> Graph.addEdge (A .>. B)
    |-> Graph.addEdge (B .>. C)
    |--> Graph.addNode D 

let consMapping m = m |> List.map (fun (k,v) -> Value (String k), Value (String v)) |> Map.ofList
let consMappings mapLst = List.map consMapping mapLst

[<Tests>]
let test =
    // Graph is ["b"::20, >, "a"::10, >, "d"::40]
    let testG =
        Graph.empty
        |> Graph.addEdge (b .>. a)
        |--> Graph.addNode d

    let mappings = [
        [("b","A"); ("a","B"); ("d","C";)]
        [("b","A"); ("a","B"); ("d","D";)]
        [("b","B"); ("a","C"); ("d","A";)]
        [("b","B"); ("a","C"); ("d","D";)]
    ]
    let expMappings = consMappings mappings |> Set.ofList
    let gotMappings = Graph.sgiAll mainG testG
    testCase "Subgraph isomorphism enumeration test" <| fun _ ->
        Expect.equal gotMappings expMappings ""
 
[<Tests>]
let test2 =
    // Graph is ["b"::20, >, "a"::10, >, "d"::40]
    let testG =
        Graph.empty
        |> Graph.addEdge (b .>. a)
        |--> Graph.addEdge (a .>. d)

    let res = Graph.sgiFirst mainG testG
    let mapping = [("b","A"); ("a","B"); ("d","C")]
    let expMapping = mapping |> consMapping
    testCase "Basic subgraph isomorphism test" <| fun _ ->
        match res with
        | Some gotMapping -> Expect.equal gotMapping expMapping ""
        | None -> failwithf "No subgraph isomorphism was found"