module Bilbo.Graph.Graph

open Bilbo.Common.Extensions
open Bilbo.Common.Value
open Bilbo.Common.Error
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

    let addNode (n : Node) (g : Graph) : BilboResult<Graph> =
        let nodesStart = g.nodes
        let nodesNew load = Map.add n.id load nodesStart
        let gNew load = {g with nodes = nodesNew load}
        match n.id with
        | ParamList _ -> paramListImpError "a node id"
        | Space (Namespace, n) -> bindImpError "namespace" "a node id"
        // TODO: Others!!!
        | Value (Graph g) -> bindImpError "graph" "a node id"
        | _ ->
            match n.load with
            | ParamList _ -> paramListImpError "a node load"
            | Space (Namespace, n) -> bindImpError "namespace" "a node load"
            // TODO: Others!!!
            | Value (Graph g) -> bindImpError "graph" "a node load"
            | _ -> gNew n.load |> Ok
            
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
        let folder (g : BilboResult<Graph>) n =
            match g with
            | Error e -> e |> Error
            | Ok g' -> addNode n g'
        List.fold folder (Ok g)  nLst

    let addEdge (e : Edge) (g : Graph) =
        let id = g.edgeIdCount
        g
        |> addNode e.source
        |-> addNode e.target
        |=> fun g' ->
            let eInfo = {EdgeInfo.source=e.source.id; weight=e.weight; target=e.target.id; id=id}
            let edges' = Map.add id eInfo  g'.edges
            {g' with edges=edges'; edgeIdCount=id+1}

    let addEdges (eLst : Edge list) (g : Graph) =
        let folder (g : BilboResult<Graph>) edge =
            match g with
            | Error e -> e |> Error
            | Ok g' -> addEdge edge g'
        List.fold folder (Ok g) eLst

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
                    
    let addGraphs (g1 : Graph) (g2 : Graph) : BilboResult<Graph> =
        empty
        |> addNodes (nodes g1)
        |-> addEdges (edges g1)
        |-> addNodes (nodes g2)
        |-> addEdges (edges g2)

    let subtractGraphs (g1 : Graph) (g2 : Graph) : BilboResult<Graph> =
        // Implements
        //  e = e1 - e2
        //  n = n1 - (n2 - nodes(e2))
        let e1 = edges g1
        let e2 = edges g2
        let e = e1 /-/ e2
        let n1 = nodes g1 |> Set.ofList
        let n2 = nodes g2 |> Set.ofList
        let ne2 : Set<Node> =
            e2
            |> List.collect (fun e -> [e.source;e.target]) 
            |> Set.ofList
        let n = n1 - (n2 - ne2) |> Set.toList
        empty
        |> addNodes n
        |-> addEdges e
        
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
        let e = e1 /-/ e2
        let n1 = ug1.nodes
        let n2 = ug2.nodes
        let ne2 =
            e2
            |> List.collect (fun e -> [e.source;e.target]) 
            |> Set.ofList
        let n = n1 - (n2 - ne2) |> Set.toList
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
            |-> Graph.addNodes mappedNodes
        (sg, revNMap, revEMap)

module PatternGraph =
    let empty = {pos=UnboundGraph.empty; neg=None}

    let addToPos pg posIn =
        {pg with pos = UnboundGraph.addGraphs pg.pos posIn}

    let addToNeg pg negIn =
        match pg with
        | {neg=Some n'; pos=_} -> {pg with neg = UnboundGraph.addGraphs n' negIn |> Some}
        | {neg=None; pos=_} -> {pg with neg = negIn |> Some} 

    let addGraphs pg1 pg2 =
        let pg' = addToPos pg1 pg2.pos
        match pg2.neg with
        | Some neg -> addToNeg pg' neg        
        | None -> pg'

    let subtractGraphs pg1 pg2 =
        let pg' = {pg1 with pos = UnboundGraph.subtractGraphs pg1.pos pg2.pos}
        match pg1.neg, pg2.neg with
        | Some neg1, Some neg2 -> {pg' with neg = UnboundGraph.subtractGraphs neg1 neg2 |> Some}
        | _, _ -> pg'