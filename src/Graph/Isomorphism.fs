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
    | Some _  -> true
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
        let subGNodes = Graph.nodes subgraph |> List.map (fun n -> n.id)
        nodeSgiSearch hostGraph subgraph subGNodes Map.empty folder

    let sgiFirst hostGraph subgraph =
        let rec edgeSearch hg sg (sgEdgesLeft : EdgeId list) nMapping eMapping =
            let mapNode (n : Node) = Map.find n.id nMapping |> fun i -> Graph.node i hg
            match sgEdgesLeft with
            | [] -> eMapping |> Some
            | _ ->
                let allHgEdges = hg |> Graph.idEdges |> List.map fst
                let hgEdgeIsMappedTo eid = isMappedTo eid eMapping
                let unmappedHgEdges = List.filter (hgEdgeIsMappedTo >> not) allHgEdges
                let sgEdgesToBeMapped =
                    List.map (fun eid -> (eid, List.filter (fun e2 -> e2 <> eid) sgEdgesLeft)) sgEdgesLeft
                let possibleEdgeMappings = List.allPairs sgEdgesToBeMapped unmappedHgEdges
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
                | [] -> None
                | _ ->
                    let folder currentMapping ((sgEdgeToTry,sgEdgesNowLeft), hgEdgeToTry) = 
                        match currentMapping with
                        | Some a -> a |> Some
                        | None ->
                            let mappingAttempt = Map.add sgEdgeToTry hgEdgeToTry eMapping
                            let searchRes = edgeSearch hg sg sgEdgesNowLeft nMapping mappingAttempt
                            searchRes
                    List.fold folder None consistentEdgeMappings                                   
        let nMaps = nodeSgiAll hostGraph subgraph |> Set.toList
        match nMaps with
        | [] -> None
        | _ ->
            let subGEdges = subgraph |> Graph.idEdges |> List.map fst
            let folder (prevNMap,eMap) nMap =
                match eMap with
                | Some eMap' -> (prevNMap, Some eMap')
                | None ->
                    nMap, edgeSearch hostGraph subgraph subGEdges nMap Map.empty
            let eMapRes =  List.fold folder (Map.empty,None) nMaps
            match eMapRes with
            | nMap, Some eMap -> (nMap,eMap) |> Some
            | _, None -> None

    let sgiAll hostGraph subgraph =
        let rec edgeSearch hg sg (sgEdgesLeft : EdgeId list) nMapping eMapping =
            let mapNode (n : Node) = Map.find n.id nMapping |> fun i -> Graph.node i hg
            match sgEdgesLeft with
            | [] -> eMapping |> Some
            | _ ->
                let allHgEdges = hg |> Graph.idEdges |> List.map fst
                let hgEdgeIsMappedTo eid = isMappedTo eid eMapping
                let unmappedHgEdges = List.filter (hgEdgeIsMappedTo >> not) allHgEdges
                let sgEdgesToBeMapped =
                    List.map (fun eid -> (eid, List.filter (fun e2 -> e2 <> eid) sgEdgesLeft)) sgEdgesLeft
                let possibleEdgeMappings = List.allPairs sgEdgesToBeMapped unmappedHgEdges
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
                | [] -> None
                | _ ->
                    let folder currentMapping ((sgEdgeToTry,sgEdgesNowLeft), hgEdgeToTry) = 
                        match currentMapping with
                        | Some a -> a |> Some
                        | None ->
                            let mappingAttempt = Map.add sgEdgeToTry hgEdgeToTry eMapping
                            let searchRes = edgeSearch hg sg sgEdgesNowLeft nMapping mappingAttempt
                            searchRes
                    List.fold folder None consistentEdgeMappings

        let nMaps = nodeSgiAll hostGraph subgraph |> Set.toList
        match nMaps with
        | [] -> [] |> Set.ofList
        | _ ->
            let subGEdges = subgraph |> Graph.idEdges |> List.map fst
            let folder neMaps nMap =
                let eMapRes = edgeSearch hostGraph subgraph subGEdges nMap Map.empty
                match eMapRes with
                | None -> neMaps
                | Some eMap -> (nMap,eMap) :: neMaps
            List.fold folder [] nMaps
            |> Set.ofList