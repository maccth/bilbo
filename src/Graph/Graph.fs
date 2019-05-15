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
        |=> fun g' -> g',(Map.add id e g'.edges)
        |=> fun (g',edges') -> {g' with edges=edges'; edgeIdCount=id+1}

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

    let edge id (g : Graph) =
        g.edges
        |> Map.find id
   
    let nodes (g : Graph) =
        g.nodes
        |> Map.toList
        |> List.map (fun (id,load) -> {id=id; load=load})

    let idEdges (g : Graph) =
        g.edges
        |> Map.toList

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
        let e2 = edges g2
        let n1 = nodes g1 |> Set.ofList
        let n2 = nodes g2 |> Set.ofList
        let ne2 : Set<Node> =
            e2
            |> List.collect (fun e -> [e.source;e.target]) 
            |> Set.ofList
        let e = edges g1 /-/ e2
        let n = n1 - (n2 - ne2) |> Set.toList
        let gN = addNodes n empty
        let gNE = Result.bind (fun g -> addEdges e g) gN
        gNE
        
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

    let empty : UnboundGraph = {nodes=Set.empty; edges=Set.empty; edgeIdCount=0}

    let addNode (n : UnboundNode) (ug : UnboundGraph) : UnboundGraph =
        {ug with nodes = Set.add n ug.nodes}

    let addNodes (nLst : UnboundNode list) (ug : UnboundGraph) : UnboundGraph =
        {ug with nodes = (Set.ofList nLst) + ug.nodes}

    let addEdge (e : UnboundEdge) (ug : UnboundGraph) =
        let id = ug.edgeIdCount
        ug
        |> addNode e.source
        |> addNode e.target
        |> fun ug' -> ug',(Set.add (id,e)  ug'.edges)
        |> fun (ug'',edges') -> {ug'' with edges=edges'; edgeIdCount=id+1}
        
    let addEdges (eLst : UnboundEdge list) (ug : UnboundGraph) : UnboundGraph =
        List.fold (fun ug' e -> addEdge e ug') ug eLst

    let edge (id : UnboundEdgeId) (ug : UnboundGraph) =
        ug.edges
        |> Set.toList
        |> Map.ofList
        |> Map.find id
    
    let nodes (ug : UnboundGraph) =
        ug.nodes |> Set.toList

    let idEdges (ug : UnboundGraph) =
        ug.edges
        |> Set.toList
    
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
        let e2 = edges ug2
        let n1 = ug1.nodes
        let n2 = ug2.nodes
        let ne2 =
            e2
            |> List.collect (fun e -> [e.source;e.target]) 
            |> Set.ofList
        let e = edges ug1 /-/ e2
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