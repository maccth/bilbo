module Bilbo.Graph.Isomorphism

open Bilbo.Common.Value
open Bilbo.Graph.Graph

let isMappedTo elem m =
    Map.tryFindKey (fun k v -> v=elem) m
    |> function
    | Some _ -> true
    | None -> false

let isMappedFrom elem m =
    Map.tryFind elem m
    |> function
    | Some _ -> true
    | None -> false


module Graph =
    let rec nodeSgiSearch hg sg sgNodesLeft mapping folder =
        let mapNode (n : Node) = Map.find n.id mapping |> fun i -> Graph.node i hg
        let mapEdge (e : Edge) = {e with source = mapNode e.source; target = mapNode e.target; weight=None}
        let edgesInSg = Graph.edges sg
        let edgeInCurrentSg (e : Edge) = (Map.containsKey e.source.id mapping) && (Map.containsKey e.target.id mapping)
        let edgesInCurrentSg = List.filter edgeInCurrentSg edgesInSg
        let mappedEdgesInCurrentSg = List.map mapEdge edgesInCurrentSg
        let edgesInHg = Graph.edges hg |> List.map (fun e -> {e with weight=None}) |> set
        match Set.isSubset (set mappedEdgesInCurrentSg) edgesInHg with
        | false -> Set.empty
        | true -> 
            match sgNodesLeft with
            | [] ->
                match edgesInSg = edgesInCurrentSg with
                | false -> Set.empty
                | true -> Set.ofList [mapping]
            | _ ->
                let allHgNodes = hg |> Graph.nodes |> List.map (fun n -> n.id)
                let hgNodeMappedTo id = isMappedTo id mapping
                let hgNodesNotMappedTo = List.filter (hgNodeMappedTo >> not) allHgNodes
                let sgNodesToBeMatched = List.map (fun n -> (n, List.filter (fun n2 -> n2 <> n) sgNodesLeft)) sgNodesLeft
                let possibleMappingPairs = List.allPairs sgNodesToBeMatched hgNodesNotMappedTo
                List.fold (folder mapping) Set.empty possibleMappingPairs

    let rec edgeSgiSearch hg sg (sgEdgesLeft : EdgeId list) nMapping eMapping folder =
        match sgEdgesLeft with
        | [] -> Set.ofList [eMapping]
        | _ ->
            let mapNode (n : Node) = Map.find n.id nMapping |> fun i -> Graph.node i hg
            let allHgEdges = hg |> Graph.idEdges |> List.map fst
            let hgEdgeMappedTo eid = isMappedTo eid eMapping
            let hgEdgesNotMappedTo = List.filter (hgEdgeMappedTo >> not) allHgEdges
            let sgEdgesToBeMapped =
                List.map (fun eid -> (eid, List.filter (fun e2 -> e2 <> eid) sgEdgesLeft)) sgEdgesLeft
            let possibleEdgeMappings = List.allPairs sgEdgesToBeMapped hgEdgesNotMappedTo
            let consistentMapping sgEId hgEId =
                let sgE = Graph.edge sgEId sg
                let hgE = Graph.edge hgEId hg
                let weightIso =
                    match sgE.weight, hgE.weight with
                    | Some _, Some _ -> true
                    | None, None -> true
                    | _ -> false
                let sgEndpoints = (sgE.source |> mapNode, sgE.target |> mapNode)
                let hgEndpoints = (hgE.source, hgE.target)
                (sgEndpoints = hgEndpoints) && weightIso
            let consistentEdgeMappings =
                possibleEdgeMappings
                |> List.filter (fun ((sge,_), hge) -> consistentMapping sge hge)
            match consistentEdgeMappings with
            | [] -> Set.empty
            | _ ->
                List.fold (folder nMapping eMapping) Set.empty consistentEdgeMappings

    let nodeSgiAll hostGraph subgraph =
        let rec folder nodeMappings validMappings ((sgNodeToTry,sgNodesNowLeft), hgNodeToTry) =
            let mappingAttempt = Map.add sgNodeToTry hgNodeToTry nodeMappings
            let searchRes = nodeSgiSearch hostGraph subgraph sgNodesNowLeft mappingAttempt folder
            searchRes + validMappings
        let sgNodes = Graph.nodes subgraph |> List.map (fun n -> n.id)
        nodeSgiSearch hostGraph subgraph sgNodes Map.empty folder  

    let nodeSgiFirst hostGraph subgraph =
        let rec folder nodeMappings currentMapping ((sgNodeToTry,sgNodesNowLeft), hgNodeToTry) =
            if currentMapping <> Set.empty then
                currentMapping
            else
                let mappingAttempt = Map.add sgNodeToTry hgNodeToTry nodeMappings
                let searchRes = nodeSgiSearch hostGraph subgraph sgNodesNowLeft mappingAttempt folder
                searchRes
        let sgNodes = Graph.nodes subgraph |> List.map (fun n -> n.id)
        nodeSgiSearch hostGraph subgraph sgNodes Map.empty folder

    let sgiFirst hostGraph subgraph =                                
        let rec eMapfolder nMapping eMapping currentMapping ((sgEdgeToTry,sgEdgesNowLeft), hgEdgeToTry) = 
            if currentMapping <> Set.empty then
                currentMapping
            else                        
                let mappingAttempt = Map.add sgEdgeToTry hgEdgeToTry eMapping
                let searchRes = edgeSgiSearch hostGraph subgraph sgEdgesNowLeft nMapping mappingAttempt eMapfolder
                searchRes
        let nMaps = nodeSgiAll hostGraph subgraph |> Set.toList
        match nMaps with
        | [] -> Set.empty
        | _ ->
            let subGEdges = subgraph |> Graph.idEdges |> List.map fst
            let folder (mapsSet : Set<Map<NodeId,NodeId>*Map<EdgeId,EdgeId>>) nMap =
                match mapsSet <> Set.empty with
                | true -> mapsSet
                | false ->
                    let eMaps = edgeSgiSearch hostGraph subgraph subGEdges nMap Map.empty eMapfolder
                    Set.map (fun em -> (nMap,em)) eMaps
            List.fold folder Set.empty nMaps

    let sgiAll hostGraph subgraph =
        let rec eMapFolder nMapping eMapping validMappings ((sgEdgeToTry,sgEdgesNowLeft), hgEdgeToTry) = 
            let mappingAttempt = Map.add sgEdgeToTry hgEdgeToTry eMapping
            let searchRes = edgeSgiSearch hostGraph subgraph sgEdgesNowLeft nMapping mappingAttempt eMapFolder
            searchRes + validMappings
        let nMaps = nodeSgiAll hostGraph subgraph |> Set.toList
        match nMaps with
        | [] -> [] |> Set.ofList
        | _ ->
            let subGEdges = subgraph |> Graph.idEdges |> List.map fst
            let folder neMaps nMap =
                let eMapRes = edgeSgiSearch hostGraph subgraph subGEdges nMap Map.empty eMapFolder
                neMaps + Set.map (fun eMap -> (nMap,eMap)) eMapRes
            List.fold folder Set.empty nMaps

    let rec unboundNodeSgiSearch (hg : Graph) (sg : UnboundGraph) (sgNodesLeft : UnboundNodeId list) (mapping : Map<UnboundNodeId,NodeId>) folder : Set<Map<UnboundNodeId,NodeId>> =
        let mapNode (n : UnboundNode) = Map.find n.nid mapping |> fun i -> Graph.node i hg
        let mapEdge (e : UnboundEdge) : Edge = {source = mapNode e.source; target = mapNode e.target; weight=None}
        let edgesInSg = UnboundGraph.edges sg
        let edgeInCurrentSg (e : UnboundEdge) = (Map.containsKey e.source.nid mapping) && (Map.containsKey e.target.nid mapping)
        let edgesInCurrentSg = List.filter edgeInCurrentSg edgesInSg
        let mappedEdgesInCurrentSg = List.map mapEdge edgesInCurrentSg
        let edgesInHg = Graph.edges hg |> List.map (fun e -> {e with weight=None}) |> set
        match Set.isSubset (set mappedEdgesInCurrentSg) edgesInHg with
        | false -> Set.empty
        | true -> 
            match sgNodesLeft with
            | [] ->
                match edgesInSg = edgesInCurrentSg with
                | false -> Set.empty
                | true -> Set.ofList [mapping]
            | _ ->
                let allHgNodes = hg |> Graph.nodes |> List.map (fun n -> n.id)
                let hgNodeMappedTo id = isMappedTo id mapping
                let hgNodesNotMappedTo = List.filter (hgNodeMappedTo >> not) allHgNodes
                let sgNodesToBeMatched = List.map (fun n -> (n, List.filter (fun n2 -> n2 <> n) sgNodesLeft)) sgNodesLeft
                let possibleMappingPairs = List.allPairs sgNodesToBeMatched hgNodesNotMappedTo
                List.fold (folder mapping) Set.empty possibleMappingPairs

    let rec unboundEdgeSgiSearch (hg : Graph) (sg : UnboundGraph) (sgEdgesLeft : UnboundEdgeId list) nMapping (eMapping : Map<UnboundEdgeId,EdgeId>) folder =
        match sgEdgesLeft with
        | [] -> Set.ofList [eMapping]
        | _ ->
            let mapNode (n : UnboundNode) = Map.find n.nid nMapping |> fun i -> Graph.node i hg
            let allHgEdges = hg |> Graph.idEdges |> List.map fst
            let hgEdgeMappedTo eid = isMappedTo eid eMapping
            let hgEdgesNotMappedTo = List.filter (hgEdgeMappedTo >> not) allHgEdges
            let sgEdgesToBeMapped =
                List.map (fun eid -> (eid, List.filter (fun e2 -> e2 <> eid) sgEdgesLeft)) sgEdgesLeft
            let possibleEdgeMappings = List.allPairs sgEdgesToBeMapped hgEdgesNotMappedTo
            let consistentMapping sgEId hgEId =
                let sgE = UnboundGraph.edge sgEId sg
                let hgE = Graph.edge hgEId hg
                let weightIso =
                    match sgE.weight, hgE.weight with
                    | Some _, Some _ -> true
                    | None, None -> true
                    | _ -> false
                let sgEndpoints = (sgE.source |> mapNode, sgE.target |> mapNode)
                let hgEndpoints = (hgE.source, hgE.target)
                (sgEndpoints = hgEndpoints) && weightIso
            let consistentEdgeMappings =
                possibleEdgeMappings
                |> List.filter (fun ((sge,_), hge) -> consistentMapping sge hge)
            match consistentEdgeMappings with
            | [] -> Set.empty
            | _ ->
                List.fold (folder nMapping eMapping) Set.empty consistentEdgeMappings

    let unboundNodeSgiAll (hostGraph : Graph) (subgraph : UnboundGraph) =
        let rec folder nodeMappings validMappings ((sgNodeToTry,sgNodesNowLeft), hgNodeToTry) =
            let mappingAttempt = Map.add sgNodeToTry hgNodeToTry nodeMappings
            let searchRes = unboundNodeSgiSearch hostGraph subgraph sgNodesNowLeft mappingAttempt folder
            searchRes + validMappings
        let sgNodes = UnboundGraph.nodes subgraph |> List.map (fun n -> n.nid)
        unboundNodeSgiSearch hostGraph subgraph sgNodes Map.empty folder  

    let unboundNodeSgiFirst (hostGraph : Graph) (subgraph : UnboundGraph) =
        let rec folder nodeMappings currentMapping ((sgNodeToTry,sgNodesNowLeft), hgNodeToTry) =
            if currentMapping <> Set.empty then
                currentMapping
            else
                let mappingAttempt = Map.add sgNodeToTry hgNodeToTry nodeMappings
                let searchRes = unboundNodeSgiSearch hostGraph subgraph sgNodesNowLeft mappingAttempt folder
                searchRes
        let sgNodes = UnboundGraph.nodes subgraph |> List.map (fun n -> n.nid)
        unboundNodeSgiSearch hostGraph subgraph sgNodes Map.empty folder  

    let unboundSgiFirst (hostGraph : Graph) (subgraph : UnboundGraph) : Set<Map<UnboundNodeId,NodeId>*Map<UnboundEdgeId,EdgeId>> =                                
        let rec eMapfolder nMapping eMapping currentMapping ((sgEdgeToTry,sgEdgesNowLeft), hgEdgeToTry) = 
            if currentMapping <> Set.empty then
                currentMapping
            else                        
                let mappingAttempt = Map.add sgEdgeToTry hgEdgeToTry eMapping
                let searchRes = unboundEdgeSgiSearch hostGraph subgraph sgEdgesNowLeft nMapping mappingAttempt eMapfolder
                searchRes
        let nMaps = unboundNodeSgiAll hostGraph subgraph |> Set.toList
        match nMaps with
        | [] -> Set.empty
        | _ ->
            let subGEdges = subgraph |> UnboundGraph.idEdges |> List.map fst
            let folder (mapsSet : Set<Map<NodeId,NodeId>*Map<EdgeId,EdgeId>>) nMap =
                match mapsSet <> Set.empty with
                | true -> mapsSet
                | false ->
                    let eMaps = unboundEdgeSgiSearch hostGraph subgraph subGEdges nMap Map.empty eMapfolder
                    Set.map (fun em -> (nMap,em)) eMaps
            List.fold folder Set.empty nMaps

    let unboundSgiAll (hostGraph : Graph) (subgraph : UnboundGraph) : Set<Map<UnboundNodeId,NodeId>*Map<UnboundEdgeId,EdgeId>> =
        let rec eMapFolder nMapping eMapping validMappings ((sgEdgeToTry,sgEdgesNowLeft), hgEdgeToTry) = 
            let mappingAttempt = Map.add sgEdgeToTry hgEdgeToTry eMapping
            let searchRes = unboundEdgeSgiSearch hostGraph subgraph sgEdgesNowLeft nMapping mappingAttempt eMapFolder
            searchRes + validMappings
        let nMaps = unboundNodeSgiAll hostGraph subgraph |> Set.toList
        match nMaps with
        | [] -> [] |> Set.ofList
        | _ ->
            let subGEdges = subgraph |> UnboundGraph.idEdges |> List.map fst
            let folder neMaps nMap =
                let eMapRes = unboundEdgeSgiSearch hostGraph subgraph subGEdges nMap Map.empty eMapFolder
                neMaps + Set.map (fun eMap -> (nMap,eMap)) eMapRes
            List.fold folder Set.empty nMaps