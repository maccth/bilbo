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
    (d, None,       a)
    (c, Some 30,    a)
]

// let f = {
//     nodes = Map.ofList [
//         (Value (String "a"), Value (Int 1));
//         (Value (String "b"), Value (Int 1));
//         (Value (String "c"), Value (Int 1));
//         (Value (String "d"), Value (Int 1))];

//     sourceEdges = Map.ofList [
//         (Value (String "c"),
//             Map.ofList [(Value (String "a"), [Some (Value (Int 30))])])];

//     targetEdges = Map.ofList
//         [(Value (String "a"),
//             Map.ofList[(Value (String "c"), [Some (Value (Int 30))]); (Value (String "d"), [None])])];
// }

let gExp =
    {
        nodes = ["a"; "b"; "c"; "d"] |> List.map (fun id -> (id |> bilboStr, bilbo1)) |> Map.ofList
        sourceEdges =  Map.ofList [
            bilboStr "a",
                Map.ofList [bilboStr "b", [40 |> bilboInt |> Some; 200 |> bilboInt |> Some]];
            bilboStr "b",
                Map.ofList [bilboStr "d", [None]; bilboStr "c", [15 |> bilboInt |> Some]];
            bilboStr "d",
                Map.ofList [bilboStr "a",[None]];
            bilboStr "c",
                Map.ofList [bilboStr "a", [30 |> bilboInt |> Some]];                    
        ];
        targetEdges = Map.ofList [
            bilboStr "b",
                Map.ofList [bilboStr "a", [40 |> bilboInt |> Some; 200 |> bilboInt |> Some]];
            bilboStr "d",
                Map.ofList [bilboStr "b", [None]];
            bilboStr "c",
                Map.ofList [bilboStr "a", [15 |> bilboInt |> Some]];        
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