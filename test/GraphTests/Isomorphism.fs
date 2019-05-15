module Bilbo.Tests.GraphTests.Isomorphism

open Expecto
open Bilbo.Common.Extensions
open Bilbo.Common.Value
open Bilbo.Graph.Graph
open Bilbo.Graph.Isomorphism
open Bilbo.Tests.GraphTests.Helpers

let consNMapping m = m |> List.map (fun (k,v) -> Value (String k), Value (String v)) |> Map.ofList
let consNMappings mapLst = List.map consNMapping mapLst
let consEMapping m : Map<EdgeId,EdgeId> = m |> Map.ofList

let A = ndSi "A" 1
let B = ndSi "B" 2
let C = ndSi "C" 3
let D = ndSi "D" 4

let a = ndSi "a" 10
let b = ndSi "b" 20
let c = ndSi "c" 30
let d = ndSi "d" 40


// [A, >, B, >, C, D]
let mainHostG =
    Graph.empty
    |> Graph.addEdge (A .>. B)
    |-> Graph.addEdge (B .>. C)
    |--> Graph.addNode D 

[<Tests>]
let test =
    // [b, >, a, >, d]
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
    let expMappings = consNMappings mappings |> Set.ofList
    let gotMappings = Graph.nodeSgiAll mainHostG testG
    testCase "Node-induced SGI enumeration test" <| fun _ ->
        Expect.equal gotMappings expMappings ""
 
[<Tests>]
let test2 =
    // [b, >, a, >, d]
    let testG =
        Graph.empty
        |> Graph.addEdge (b .>. a)
        |--> Graph.addEdge (a .>. d)
    let gotMapping = Graph.nodeSgiFirst mainHostG testG
    let mapping = [("b","A"); ("a","B"); ("d","C")]
    let expMapping = mapping |> consNMapping |> fun m -> Set.ofList [m]
    testCase "Node-induced SGI test, one iso" <| fun _ ->
        Expect.equal gotMapping expMapping ""

[<Tests>]
let test3 =
    // [b, 1>, a, 1>, d]
    let testG =
        Graph.empty
        |> Graph.addEdge ((b ..>. a) b1)
        |--> Graph.addEdge ((a ..>. d) b1)
    let gotMapping = Graph.nodeSgiFirst mainHostG testG
    let mapping = [("b","A"); ("a","B"); ("d","C")]
    let expMapping = mapping |> consNMapping |> fun m -> Set.ofList [m]
    testCase "Node-induced SGI test, unweighetd vs weighted edges." <| fun _ ->
        Expect.equal gotMapping expMapping ""

[<Tests>]
let test4 =
    // [A, 20>, B, 20>, C, D]
    let weightedHostG =
        Graph.empty
        |> Graph.addEdge ((A ..>. B) (bInt 20))
        |-> Graph.addEdge ((B ..>. C) (bInt 20))
        |--> Graph.addNode D 
    // [b, 1>, a, 1>, d]
    let testG =
        Graph.empty
        |> Graph.addEdge ((b ..>. a) b1)
        |--> Graph.addEdge ((a ..>. d) b1)
    let gotMapping = Graph.sgiFirst weightedHostG testG    
    let nMap = [("b","A"); ("a","B"); ("d","C")]
    let expNMap = nMap |> consNMapping
    let expEMap = [(0,0); (1,1)] |> consEMapping
    let expMap = (Some (expNMap, expEMap))
    testCase "SGI test, weighted vs. weighetd edges, different weight." <| fun _ ->
        Expect.equal gotMapping expMap ""

[<Tests>]
let test5 =
    // [A, 2>, B, 27>, C, "D":4]
    let weightedHostG =
        Graph.empty
        |> Graph.addEdge ((A ..>. B) (bInt 2))
        |-> Graph.addEdge ((B ..>. C) (bInt 7))
        |--> Graph.addNode D 
    // [b, 5>, a, 1>, d]
    let testG =
        Graph.empty
        |> Graph.addEdge ((b ..>. a) (bInt 5))
        |--> Graph.addEdge ((a ..>. d) b1)
    let gotMapping = Graph.sgiFirst weightedHostG testG    
    let nMap = [("b","A"); ("a","B"); ("d","C")]
    let expNMap = nMap |> consNMapping
    let expEMap = [(0,0); (1,1)] |> consEMapping
    let expMap = (Some (expNMap, expEMap))
    testCase "SGI test, weighted vs. weighetd edges, even more different weight." <| fun _ ->
        Expect.equal gotMapping expMap ""

[<Tests>]
let test6 =
    // [A, >, B, >, C] + [B, 7>, C] + [D]
    let weightedHostG =
        Graph.empty
        |> Graph.addEdge (A .>. B) 
        |-> Graph.addEdge (B .>. C) 
        |-> Graph.addEdge ((B ..>. C) (bInt 7))
        |--> Graph.addNode D 
    // [a,>,b] + [b,>,d] + [b,100>,d]
    let testG =
        Graph.empty
        |> Graph.addEdge (b .>. d)
        |-> Graph.addEdge ((b ..>. d) (bInt 100))
        |--> Graph.addEdge (a .>. b)
    let expNMap = [("a","A"); ("b","B"); ("d","C")] |> consNMapping
    let expEMap = [(0,1); (1,2); (2,0)] |> consEMapping
    let expMap = (Some (expNMap, expEMap))    
    let gotMapping = Graph.sgiFirst weightedHostG testG    
    testCase "SGI test, mixed weighted/unweighted with valid iso." <| fun _ ->
        Expect.equal gotMapping expMap ""

[<Tests>]
let test7 =
    // [A, >, B, >, C] + [B, 7>, C] + [D]
    let weightedHostG =
        Graph.empty
        |> Graph.addEdge (A .>. B) 
        |-> Graph.addEdge (B .>. C) 
        |-> Graph.addEdge ((B ..>. C) (bInt 7))
        |--> Graph.addNode D 
    // [a,>,b] + [c]
    let testG =
        Graph.empty
        |> Graph.addEdge (a .>. b)
        |--> Graph.addNode c
    let expMaps =
        [
            ["a","A"; "b","B"; "c","C"], [0,0]
            ["a","A"; "b","B"; "c","D"], [0,0]

            ["a","B"; "b","C"; "c","D"], [0,1]
            ["a","B"; "b","C"; "c","A"], [0,1]
        ]
        |> List.map (fun (nMap,eMap) -> consNMapping nMap, consEMapping eMap)
        |> Set.ofList
    let gotMapping = Graph.sgiAll weightedHostG testG    
    testCase "SGI enumeration test with mixed weighted/unweighted edges in host and pattern. 4 valis isos." <| fun _ ->
        Expect.equal (set gotMapping) expMaps ""