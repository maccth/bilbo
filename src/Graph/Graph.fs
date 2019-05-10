module Bilbo.Graph.Graph

open Bilbo.Common.Value
open Bilbo.Common.Error
open Bilbo.Common.SymbolTable

module Graph =

    let empty : Graph = {nodes=Map.empty; sourceEdges=Map.empty; targetEdges=Map.empty} 

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

    let addEdge (g : Graph) (e : Edge) : BilboResult<Graph> =
        let addOneWayEdge (eMap : EdgeMap<EdgeMap<EdgeWeight list>>) (l1nid : NodeId) (l2nid : NodeId) (ew : EdgeWeight) =
            match Map.tryFind l1nid eMap with
            | None ->
                Map.empty
                |> fun m -> Map.add l2nid [ew] m
                |> fun m -> Map.add l1nid m Map.empty
            | Some l2eMap ->
                match Map.tryFind l2nid l2eMap with
                | None ->
                    Map.add l2nid [ew] l2eMap
                    |> fun l2eMap' -> Map.add l1nid l2eMap' eMap
                | Some ewLst ->
                    ew :: ewLst
                    |> fun ews -> Map.add l2nid ews l2eMap
                    |> fun l2eMap' -> Map.add l1nid l2eMap' eMap

        let gWithNodes = addNodes g [e.source; e.target]
        match gWithNodes with
        | Error e -> e |> Error
        | Ok gOk ->
            gOk
            |> fun g1 -> (addOneWayEdge g1.sourceEdges e.source.id e.target.id e.weight),g1
            |> fun (sourceEMap,g2) -> {g2 with sourceEdges=sourceEMap}
            |> fun g3 -> (addOneWayEdge g3.targetEdges e.target.id e.source.id e.weight),g3
            |> fun (targetEMap,g4) -> {g4 with targetEdges=targetEMap}
            |> Ok

    let addEdges (g : Graph) (eLst : Edge list) =
        let folder (g : BilboResult<Graph>) edge =
            match g with
            | Error e -> e |> Error
            | Ok g' -> addEdge g' edge
        List.fold folder (Ok g) eLst