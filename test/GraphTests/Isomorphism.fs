module Bilbo.Tests.GraphTests.Isomorphism

open Expecto
open Bilbo.Common.Value
open Bilbo.Graph.Graph
open Bilbo.Graph.Isomorphism

// Graph is ["A"::1, >, "B"::2, >, "C"::3, "D":4]
let mainG = {
    nodes = Map.ofList
              [(Value (String "A"), Value (Int 1));
               (Value (String "B"), Value (Int 2));
               (Value (String "C"), Value (Int 3));
               (Value (String "D"), Value (Int 4))];
   sourceEdges =
            Map.ofList
              [(Value (String "A"), Map.ofList [(Value (String "B"), [None])]);
               (Value (String "B"), Map.ofList [(Value (String "C"), [None])])];
   targetEdges =
            Map.ofList
              [(Value (String "B"), Map.ofList [(Value (String "A"), [None])]);
               (Value (String "C"), Map.ofList [(Value (String "B"), [None])])];
}

let consMapping m = m |> List.map (fun (k,v) -> Value (String k), Value (String v)) |> Map.ofList
let consMappings mapLst = List.map consMapping mapLst

[<Tests>]
let test =
    // Graph is ["b"::20, >, "a"::10, >, "d"::40]
    let testG = {
        nodes = Map.ofList
            [(Value (String "a"), Value (Int 10));
            (Value (String "b"), Value (Int 20));
            (Value (String "d"), Value (Int 40))];

        sourceEdges = Map.ofList
            [(Value (String "b"), Map.ofList [(Value (String "a"), [None])])];

        targetEdges = Map.ofList
            [(Value (String "a"), Map.ofList [(Value (String "b"), [None])]);];
    }
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
    let testG = {
        nodes = Map.ofList
            [(Value (String "a"), Value (Int 10));
            (Value (String "b"), Value (Int 20));
            (Value (String "d"), Value (Int 40))];

        sourceEdges = Map.ofList
            [(Value (String "b"), Map.ofList [(Value (String "a"), [None])]);
            (Value (String "a"), Map.ofList [(Value (String "d"), [None])]);
            ];

        targetEdges = Map.ofList
            [(Value (String "d"), Map.ofList [(Value (String "a"), [None])]);
            (Value (String "a"), Map.ofList [(Value (String "b"), [None])]);];
    }
    let res = Graph.sgiFirst mainG testG
    let mapping = [("b","A"); ("a","B"); ("d","C")]
    let expMapping = mapping |> consMapping
    testCase "Basic subgraph isomorphism test" <| fun _ ->
        match res with
        | Some gotMapping -> Expect.equal gotMapping expMapping ""
        | None -> failwithf "No subgraph isomorphism was found"