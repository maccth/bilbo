module Bilbo.Graph.Graph

open Bilbo.Common.Value
open Bilbo.Common.Error
open Bilbo.Common.SymbolTable

module Graph =

    let empty = {nodes=Map.empty; sourceEdges=Map.empty; targetEdges=Map.empty}

    let addNode (g : Graph) (n : Node) : BilboResult<Graph> =
        let nodesStart = g.nodes
        let nodesNew load = Map.add n.id load nodesStart
        let gNew load = {g with nodes = nodesNew load}
        match Map.tryFind n.id nodesStart with
        | None -> gNew n.load |> Ok
        | Some nExist ->
            match nExist with
            | ParamList _ -> paramListImpError "a node load"
            | Space (Namespace, n) -> bindImpError "namespace" "a node load"
            | Value _ -> gNew n.load |> Ok
            | Space (Object(tExist), stExist) ->
                match n.load with
                | ParamList _ -> paramListImpError "a node load"
                | Space (Namespace, n) -> bindImpError "namespace" "a node load"
                | Value _ -> gNew n.load |> Ok
                | Space (Object(tNew), stNew) ->
                    let st = SymbolTable.empty
                    let st' = SymbolTable.set st {id=tExist; spLst=[]} (Space (Object(tExist), stExist))
                    match st' with
                    | Error e -> e |> Error
                    | Ok st' ->
                        let st'' = SymbolTable.set st' {id=tNew; spLst=[]} (Space (Object(tNew), stNew))
                        match st'' with
                        | Error e -> e |> Error
                        | Ok st'' ->
                            let loadNew = (Object "Object", st'') |> Space
                            gNew loadNew |> Ok
                    
    let addNodes (g : Graph) (nLst : Node list) =
        let folder (g : BilboResult<Graph>) n =
            match g with
            | Error e -> e |> Error
            | Ok g' -> addNode g' n
        List.fold folder (Ok g)  nLst



