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
    testCase "SGI enumeration test" <| fun _ ->
        Expect.equal gotMappings expMappings ""
 
[<Tests>]
let test2 =
    // ["b"::20, >, "a"::10, >, "d"::40]
    let testG =
        Graph.empty
        |> Graph.addEdge (b .>. a)
        |--> Graph.addEdge (a .>. d)

    let res = Graph.sgiFirst mainG testG
    let mapping = [("b","A"); ("a","B"); ("d","C")]
    let expMapping = mapping |> consMapping
    testCase "SGI test, one iso" <| fun _ ->
        match res with
        | Some gotMapping -> Expect.equal gotMapping expMapping ""
        | None -> failwithf "No subgraph isomorphism was found"

[<Tests>]
let test3 =
    // ["b"::20, 1>, "a"::10, 1>, "d"::40]
    let testG =
        Graph.empty
        |> Graph.addEdge ((b ..>. a) b1)
        |--> Graph.addEdge ((a ..>. d) b1)
    let res = Graph.sgiFirst mainG testG
    testCase "SGI test, no iso due to weighted vs. unweighetd edges." <| fun _ ->
        Expect.equal res None ""

[<Tests>]
let test4 =
    // ["A"::1, 20>, "B"::2, 20>, "C"::3, "D":4]
    let weightedMainG =
        Graph.empty
        |> Graph.addEdge ((A ..>. B) (bInt 20))
        |-> Graph.addEdge ((B ..>. C) (bInt 20))
        |--> Graph.addNode D 

    // ["b"::20, 1>, "a"::10, 1>, "d"::40]
    let testG =
        Graph.empty
        |> Graph.addEdge ((b ..>. a) b1)
        |--> Graph.addEdge ((a ..>. d) b1)
    let res = Graph.sgiFirst mainG testG    
    let mapping = [("b","A"); ("a","B"); ("d","C")]
    let expMapping = mapping |> consMapping
    testCase "SGI test, iso due to weighted vs. weighetd edges, but different weight." <| fun _ ->
        match res with
        | Some gotMapping -> Expect.equal gotMapping expMapping ""
        | None -> failwithf "No subgraph isomorphism was found"