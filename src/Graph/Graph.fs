module Bilbo.Graph.Graph

open Bilbo.Common.Extensions
open Bilbo.Common.Value
open Bilbo.Common.Result
open Bilbo.Common.SymbolTable

// Haskell style list "multi-set difference"
// ==============================
// credit : https://stackoverflow.com/questions/59711/haskell-list-difference-operator-in-f
// edited by maccth
let flip f x y = f y x
let rec delete x = function
  | [] -> []
  | h :: t when x = h -> t
  | h :: t -> h :: delete x t
let inline ( /-/ ) xs ys = List.fold (flip delete) xs ys


module Graph =

    let empty : Graph = {nodes=Map.empty; edges=Map.empty; edgeIdCount=0} 

    let addNode (n : Node) (g : Graph) : Graph =
        let nodesStart = g.nodes
        let nodesNew load = Map.add n.id load nodesStart
        let gNew load = {g with nodes = nodesNew load}
        {g with Graph.nodes = Map.add n.id n.load g.nodes}

        // match n.id with
        // | ParamList _ -> paramListImpError "a node id"
        // | Space (Namespace, n) -> bindImpError "namespace" "a node id"
        // // TODO: Others!!!
        // | Value (Graph g) -> bindImpError "graph" "a node id"
        // | _ ->
        //     match n.load with
        //     | ParamList _ -> paramListImpError "a node load"
        //     | Space (Namespace, n) -> bindImpError "namespace" "a node load"
        //     // TODO: Others!!!
        //     | Value (Graph g) -> bindImpError "graph" "a node load"
        //     | _ -> gNew n.load |> Ok
            
        // match n.load with
        // | ParamList _ -> paramListImpError "a node load"
        // | Space (Namespace, n) -> bindImpError "namespace" "a node load"
        // | Value _ -> gNew n.load |> Ok
        // match Map.tryFind n.id nodesStart with
        // | None -> gNew n.load |> Ok
        // | Some nExist ->
        //     match nExist with
        //     | ParamList _ -> paramListImpError "a node load"
        //     | Space (Namespace, n) -> bindImpError "namespace" "a node load"
        //     | Value _ -> gNew n.load |> Ok
        //     | Space (Object(tExist), stExist) ->
        //         match n.load with
        //         | ParamList _ -> paramListImpError "a node load"
        //         | Space (Namespace, n) -> bindImpError "namespace" "a node load"
        //         | Value _ -> gNew n.load |> Ok
        //         | Space (Object(tNew), stNew) ->
        //             let st = SymbolTable.empty
        //             let st' = SymbolTable.set st {id=tExist; spLst=[]} (Space (Object(tExist), stExist))
        //             match st' with
        //             | Error e -> e |> Error
        //             | Ok st' ->
        //                 let st'' = SymbolTable.set st' {id=tNew; spLst=[]} (Space (Object(tNew), stNew))
        //                 match st'' with
        //                 | Error e -> e |> Error
        //                 | Ok st'' ->
        //                     let loadNew = (Object "Object", st'') |> Space
        //                     gNew loadNew |> Ok
                    
    let addNodes (nLst : Node list) (g : Graph) =
        List.fold (fun gIn nIn -> addNode nIn gIn) g nLst

    let addEdge (e : Edge) (g : Graph) =
        let id = g.edgeIdCount
        g
        |> addNode e.source
        |> addNode e.target
        |> fun g' ->
            let eInfo = {EdgeInfo.source=e.source.id; weight=e.weight; target=e.target.id; id=id}
            let edges' = Map.add id eInfo  g'.edges
            {g' with edges=edges'; edgeIdCount=id+1}

    let addEdges (eLst : Edge list) (g : Graph) =
        List.fold (fun gIn eIn -> addEdge eIn gIn) g eLst

    let node id (g : Graph) =
        g.nodes
        |> Map.find id
        |> fun load -> {id=id; load=load}

    let edge id (g : Graph) : Edge =
        g.edges
        |> Map.find id
        |> fun eInfo -> {
            source = node eInfo.source g
            weight = eInfo.weight
            target = node eInfo.target g
        }
   
    let nodes (g : Graph) =
        g.nodes
        |> Map.toList
        |> List.map (fun (id,load) -> {id=id; load=load})

    let idEdges (g : Graph) =
        g.edges
        |> Map.toList
        |> List.map (fun (eid,_) -> eid, edge eid g)

    let edges (g : Graph) : Edge list =
        idEdges g
        |> List.map (fun (id,e) -> e)
                    
    let addGraphs (g1 : Graph) (g2 : Graph) : Graph =
        empty
        |> addNodes (nodes g1)
        |> addEdges (edges g1)
        |> addNodes (nodes g2)
        |> addEdges (edges g2)

    let subtractGraphs (g1 : Graph) (g2 : Graph) : Graph =
        // Implements
        //  ePure = e1 - e2
        //  n = n1 - (n2 - nodes(e2))
        //  e = any edge in ePure for which both endpoints are in n
        let e1 = edges g1
        let e2 = edges g2
        let ePure = e1 /-/ e2
        let ids = nodes >> List.map (fun n -> n.id) >> Set.ofList
        let n1 = g1 |> ids
        let n2 = g2 |> ids
        let ne2 : Set<NodeId> =
            e2
            |> List.collect (fun e -> [e.source.id;e.target.id]) 
            |> Set.ofList
        let nIds = n1 - (n2 - ne2)
        let n = (g1 |> nodes |> List.filter (fun n -> Set.contains n.id nIds))
        let e = ePure |> List.filter (fun e -> ((Set.contains e.source.id nIds) && (Set.contains e.target.id nIds)))
        empty
        |> addNodes n
        |> addEdges e
        
    let equal (g1 : Graph) (g2 : Graph) =
        // The graphs will only have unique nodes, the set removes the issue of ordering
        let g1n = nodes g1 |> Set.ofList
        let g2n = nodes g2 |> Set.ofList
        // The edges need may not be unique, so the ordering allows comparison in the presence of
        //  duplicated edges.
        let g1e = edges g1 |> List.sort
        let g2e = edges g2 |> List.sort
        (g1n = g2n) && (g1e = g2e)


module UnboundGraph =

    let empty : UnboundGraph = {nodes=Set.empty; edges=Map.empty; edgeIdCount=0}

    let addNode (n : UnboundNode) (ug : UnboundGraph) : UnboundGraph =
        {ug with nodes = Set.add n ug.nodes}

    let addNodes (nLst : UnboundNode list) (ug : UnboundGraph) : UnboundGraph =
        {ug with nodes = (Set.ofList nLst) + ug.nodes}

    let addEdge (e : UnboundEdge) (ug : UnboundGraph) =
        let id = ug.edgeIdCount
        ug
        |> addNode e.source
        |> addNode e.target
        |> fun ug' ->
            let eInfo = {source=e.source.nid; weight=e.weight; target=e.target.nid; id=id}
            let edges' = Map.add id eInfo ug.edges
            {ug' with edges=edges'; edgeIdCount=id+1}
        
    let addEdges (eLst : UnboundEdge list) (ug : UnboundGraph) : UnboundGraph =
        List.fold (fun ug' e -> addEdge e ug') ug eLst

    let edge (id : UnboundEdgeId) (ug : UnboundGraph) =
        ug.edges
        |> Map.find id
        |> fun eInfo -> {
            source = {nid=eInfo.source}
            weight = eInfo.weight
            target = {nid=eInfo.target}
        }

    let nodes (ug : UnboundGraph) =
        ug.nodes |> Set.toList

    let idEdges (ug : UnboundGraph) =
        ug.edges
        |> Map.toList
        |> List.map (fun (eid,_) -> eid, edge eid ug)
    
    let edges (ug : UnboundGraph) =
        ug
        |> idEdges
        |> List.map (fun (id,e) -> e)

    let addGraphs (ug1 : UnboundGraph) (ug2 : UnboundGraph) =
        empty
        |> addNodes (nodes ug1)
        |> addEdges (edges ug1)    
        |> addNodes (nodes ug2)
        |> addEdges (edges ug2)

    let subtractGraphs (ug1 : UnboundGraph) (ug2 : UnboundGraph) =
        // Also implements Bilbo graph subtraction
        let e1 = edges ug1
        let e2 = edges ug2
        let ePure = e1 /-/ e2
        let ids = nodes >> List.map (fun n -> n.nid) >> Set.ofList
        let n1 = ug1 |> ids
        let n2 = ug2 |> ids
        let ne2 : Set<UnboundNodeId> =
            e2
            |> List.collect (fun e -> [e.source.nid; e.target.nid]) 
            |> Set.ofList
        let nIds = n1 - (n2 - ne2)
        let n = (ug1 |> nodes |> List.filter (fun n -> Set.contains n.nid nIds))
        let e = ePure |> List.filter (fun e -> ((Set.contains e.source.nid nIds) && (Set.contains e.target.nid nIds)))
        empty
        |> addNodes n
        |> addEdges e

    let bind (g : Graph) (ug : UnboundGraph) (nMap: Map<UnboundNodeId,NodeId>) (eMap : Map<UnboundEdgeId,EdgeId>) =
        let mapNode (un : UnboundNode) = Map.find un.nid nMap |> fun id -> Graph.node id g, (id, un.nid)
        let mapEdge (ueId : UnboundEdgeId) = Map.find ueId eMap |> fun id -> Graph.edge id g, (Graph.edge id g, ueId)
        let mappedNodes, revNMap =
            ug
            |> nodes
            |> List.map (fun n -> n |> mapNode)
            |> List.unzip
            |> fun (mN, revM) -> (mN, Map.ofList revM)
        let mappedEdges, revEMap =
            ug
            |> idEdges
            |> List.map (fst >> mapEdge)
            |> List.unzip
            |> fun (mE,revM) -> (mE, Map.ofList revM)
        let sg =        
            Graph.empty
            |> Graph.addEdges mappedEdges
            |> Graph.addNodes mappedNodes
        (sg, revNMap, revEMap)

module Collection =
    let ofList (gLst : Graph list) : Collection =
        let eq g = (g |> Graph.nodes |> Set.ofList),(g |> Graph.edges |> Set.ofList)
        gLst
        |> List.distinctBy eq
        |> Set.ofList

    let join (c1 : Collection ) (c2 : Collection) : Collection =
        c1 + c2 |> Set.toList |> ofList

    let toMeaning (c : Collection) =
        match Set.count c with
        | 1 -> c |> Set.toList |> List.head |> Graph |> Value
        | _ -> c |> Value.Collection |> Value