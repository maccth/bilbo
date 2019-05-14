module Bilbo.Graph.Graph

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

    let empty : Graph = {nodes=Map.empty; sourceEdges=Map.empty; targetEdges=Map.empty} 

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

    let addEdge (e : Edge) (g : Graph) : BilboResult<Graph> =
        let addOneWayEdge (eMap : EdgeMap<EdgeMap<EdgeWeight list>>) (l1nid : NodeId) (l2nid : NodeId) (ew : EdgeWeight) =
            match Map.tryFind l1nid eMap with
            | None ->
                Map.empty
                |> fun m1 -> Map.add l2nid [ew] m1
                |> fun m2 -> Map.add l1nid m2 eMap
            | Some l2eMap ->
                match Map.tryFind l2nid l2eMap with
                | None ->
                    Map.add l2nid [ew] l2eMap
                    |> fun l2eMap' -> Map.add l1nid l2eMap' eMap
                | Some ewLst ->
                    ew :: ewLst
                    |> fun ews -> Map.add l2nid ews l2eMap
                    |> fun l2eMap' -> Map.add l1nid l2eMap' eMap
        let gWithNodes = addNodes [e.source; e.target] g
        match gWithNodes with
        | Error e -> e |> Error
        | Ok gOk ->
            gOk
            |> fun g1 -> (addOneWayEdge g1.sourceEdges e.source.id e.target.id e.weight),g1
            |> fun (sourceEMap,g2) -> {g2 with sourceEdges=sourceEMap}
            |> fun g3 -> (addOneWayEdge g3.targetEdges e.target.id e.source.id e.weight),g3
            |> fun (targetEMap,g4) -> {g4 with targetEdges=targetEMap}
            |> Ok

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
   
    let nodes (g : Graph) =
        g.nodes
        |> Map.toList
        |> List.map (fun (id,load) -> {id=id; load=load})

    let edges (g : Graph) : Edge list =
        g.sourceEdges
        |> Map.toList
        |> fun eMapLst ->
            eMapLst
            |> List.collect (fun (s,eMap2) -> 
                eMap2 
                |> Map.toList
                |> List.map (fun (t,ews) -> (s,t,ews))
                |> List.collect (fun (s,t,ews) -> List.map (fun w -> (s,t,w)) ews)           
                )
        |> List.map (fun (s,t,w) -> {source = node s g; target = node t g; weight = w})          

    let addGraphs (g1 : Graph) (g2 : Graph) : BilboResult<Graph> =
        let gSum = addNodes (nodes g1) empty
        match gSum with
        | Error e -> e |> Error
        | Ok gs1 ->
            let gs2 = addEdges (edges g1) gs1
            match gs2 with
            | Error e -> e |> Error
            | Ok gs3 ->
                let gs4 = addNodes (nodes g2) gs3
                match gs4 with
                | Error e -> e |> Error
                | Ok gs5 ->
                    addEdges (edges g2) gs5

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
        // The graphs will have only unique nodes, the set removes the issue of ordering
        let g1n = nodes g1 |> Set.ofList
        let g2n = nodes g2 |> Set.ofList
        // The edges need to be unique, so the ordering allows comparison in the presence of
        //  duplicated edges.
        let g1e = edges g1 |> List.sort
        let g2e = edges g2 |> List.sort
        (g1n = g2n) && (g1e = g2e)


module UnboundGraph =

    let empty : UnboundGraph = {nodes=Map.empty; edges=[]}

    let addNode (n : UnboundNode) (ug : UnboundGraph) : UnboundGraph =
        let nodes' = Map.add n.nid n.vid ug.nodes
        {ug with nodes=nodes'}

    let addNodes (nLst : UnboundNode list) (ug : UnboundGraph) : UnboundGraph =
        List.fold (fun ug n -> addNode n ug) ug nLst

    let addEdge (e : UnboundEdge) (ug : UnboundGraph) : UnboundGraph =
        {ug with edges = e::ug.edges}

    let addEdges (eLst : UnboundEdge list) (ug : UnboundGraph) : UnboundGraph =
        List.fold (fun ug e -> addEdge e ug) ug eLst

    let node nid (ug : UnboundGraph) =
        ug.nodes
        |> Map.find nid
        |> fun vid -> {nid=nid; vid=vid}

    let nodes (ug : UnboundGraph) =
        ug.nodes
        |> Map.toList
        |> List.map (fun (nid,vid) -> {nid=nid; vid=vid})        
    
    let edges (ug : UnboundGraph) =
        ug.edges

    let addGraphs (ug1 : UnboundGraph) (ug2 : UnboundGraph) =
        empty
        |> addNodes (nodes ug1)
        |> addEdges (edges ug1)    
        |> addNodes (nodes ug2)
        |> addEdges (edges ug2)

    let subtractGraphs (ug1 : UnboundGraph) (ug2 : UnboundGraph) =
        // Also implements Bilbo graph subtraction
        let e2 = edges ug2
        let n1 = nodes ug1 |> Set.ofList
        let n2 = nodes ug2 |> Set.ofList
        let ne2 =
            e2
            |> List.collect (fun e -> [e.source;e.target]) 
            |> Set.ofList
        let e = edges ug1 /-/ e2
        let n = n1 - (n2 - ne2) |> Set.toList
        empty
        |> addNodes n
        |> addEdges e