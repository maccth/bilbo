module Bilbo.Tests.GraphTests.Node

open Expecto
open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.Error
open Bilbo.Common.SymbolTable
open Bilbo.Graph.Graph
open Bilbo.Common

let stringIntNodeAddTests = [
    [("London",100); ("London",100)],
        [("London",100)],
        "2 nodes same identifier, same load";

    [("London",100); ("Birmingham",70)],
        [("London",100); ("Birmingham",70)],
        "2 nodes, different identifiers";

    [("A",10); ("B",20); ("C",30)],
        [("A",10); ("B",20); ("C",30)],
        "3 nodes, different identifiers"

    [("A",10); ("B",20); ("C",30); ("A",40); ("A",50); ("B",70)],
        [("A",50); ("B",70); ("C",30)],
        "3 nodes, multiple replacements identifiers"
]

let consObject typ paramLst =
    let st = SymbolTable.empty
    let stParams = List.fold (fun st (pName,pVal) -> Map.add pName pVal st) st paramLst
    stParams
    |> fun s -> Object typ, s
    |> Space

let consIntObject typ paramLst =
    let paramLst' = List.map (fun (pName,pVal) -> (pName,pVal |> Int |> Value)) paramLst
    consObject typ paramLst'

type StringObjectNodeTest = {
    nLst    : (string * string * (string * int) list) list
    expNLst : (string * string * (string * Meaning) list) list
    des     : string
}

let stringObjectNodeAddTests = [
    {
        nLst =
            [("London", "city",     [("population",10); ("capital", 20)]);
            ("London", "location",  [("language",100); ("country", 200)])];

        expNLst =
            ["London", "Object", [
                "city",     consIntObject "city"        [("population",10); ("capital", 20)];
                "location", consIntObject "location"    [("language",100); ("country", 200)]]];

        des = "two nodes, same identifiers, object loads";
    };

    {
        nLst =
            [
                ("London", "city",      [("population",10); ("capital", 20)]);
                ("London", "location",  [("language",100); ("country", 200)]);
                ("Birmingham", "city",  [("population",30); ("capital", 40)]);
                ("Birmingham", "place", [("coastal",300); ("flag", 400)]);


            ];

        expNLst =
            [
                "London", "Object", [
                    "city",     consIntObject "city"        [("population",10); ("capital", 20)];
                    "location", consIntObject "location"    [("language",100); ("country", 200)]];

                "Birmingham", "Object", [
                    "city",     consIntObject "city"        [("population",30); ("capital", 40)];
                    "place",    consIntObject "place"       [("coastal",300); ("flag", 400)]]
            ];

        des = "4 nodes, two distinct identifiers, all object-loaded";
    }    
]


let consStringIntNodesLst lst =
    let consStringIntNodes (nLst1, nLst2,des) =
        let stringIntNode (id,load) = {id=id|>String|>Value; load=load|>Int|>Value}
        let stringIntPair (id,load) = (id|>String|>Value, load|>Int|>Value)
        let nLst1' = List.map stringIntNode nLst1
        let nMap = nLst2 |> List.map stringIntPair |> Map.ofList 
        (nLst1',nMap,des)
    List.map consStringIntNodes lst

let consStringObjNodeLst lst =
    let consStringObjNode (data:StringObjectNodeTest) =
        let nLst' = List.map (fun (id,typ,prms) -> {id=id|>String|>Value; load=consIntObject typ prms}) data.nLst
        let expNLst' =
            data.expNLst 
            |> List.map (fun (id,typ,prms) -> (id|>String|>Value, consObject typ prms))
            |> Map.ofList
        (nLst',expNLst', data.des)
    List.map consStringObjNode lst

let consAddNodeTest nLst expNLst des =
    let gGot = Graph.addNodes Graph.empty nLst
    let gExp = {Graph.empty with nodes = expNLst}
    testCase des <| fun _ ->
        match gGot with
        | Ok gGot' -> Expect.equal gGot' gExp ""
        | Error e -> failwithf "Test errored. %A" e

let consAddNodeTests tLst =
    List.map (fun (nLst,expNLst,des) -> consAddNodeTest nLst expNLst des) tLst

[<Tests>]
let test =
    testList "Node add test" (stringIntNodeAddTests |> consStringIntNodesLst |> consAddNodeTests)

[<Tests>]
let test2 =
    testList "Node add test with object loads" (stringObjectNodeAddTests |> consStringObjNodeLst |> consAddNodeTests)