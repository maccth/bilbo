module Bilbo.Graph.Isomorphism

open Bilbo.Common.Extensions
open Bilbo.Common.Value
open Bilbo.Common.Error
open Bilbo.Common.SymbolTable
open Bilbo.Graph.Graph

// TODO:
// [x] Check edges, becuase we aren't just doing node induced subgraphs
// [x] Return complete list of subgraph isomorphisms
// [x] Create basic sgi -> bool function so can do negative application conditions

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

    let sgiAll hostGraph subgraph =
        let rec search hg sg sgNodesLeft mapping =
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
                    let hgNodeAlreadyAssigned id = isMappedTo id mapping
                    let unassignedHgNodes = List.filter (hgNodeAlreadyAssigned >> not) allHgNodes
                    let sgNodesToBeMatched = List.map (fun n -> (n, List.filter (fun n2 -> n2 <> n) sgNodesLeft)) sgNodesLeft
                    let possibleAssignmentPairings = List.allPairs sgNodesToBeMatched unassignedHgNodes
                    let folder validAssignments ((sgNodeToTry,sgNodesNowLeft), hgNodeToTry) =
                        let mappingAttempt = Map.add sgNodeToTry hgNodeToTry mapping
                        let searchRes = search hg sg sgNodesNowLeft mappingAttempt
                        searchRes + validAssignments
                    List.fold folder Set.empty possibleAssignmentPairings
        let sgNodes = Graph.nodes subgraph |> List.map (fun n -> n.id)
        search hostGraph subgraph sgNodes Map.empty    

    let sgiFirst hostGraph subgraph =
        let rec search hg sg sgNodesLeft mapping =
            let mapNode (n : Node) = Map.find n.id mapping |> fun i -> Graph.node i hg
            let mapEdge (e : Edge) = {e with source = mapNode e.source; target = mapNode e.target; weight=None}
            let edgesInSg = Graph.edges sg
            let edgeInCurrentSg (e : Edge) = (Map.containsKey e.source.id mapping) && (Map.containsKey e.target.id mapping)
            let edgesInCurrentSg = List.filter edgeInCurrentSg edgesInSg
            let mappedEdgesInCurrentSg = List.map mapEdge edgesInCurrentSg
            let edgesInHg = Graph.edges hg |> List.map (fun e -> {e with weight=None}) |> set
            match Set.isSubset (set mappedEdgesInCurrentSg) edgesInHg with
            | false -> None
            | true -> 
                match sgNodesLeft with
                | [] ->
                    match edgesInSg = edgesInCurrentSg with
                    | false -> None
                    | true -> mapping |> Some
                | _ ->
                    let allHgNodes = hg |> Graph.nodes |> List.map (fun n -> n.id)
                    let hgNodeAlreadyAssigned id = isMappedTo id mapping
                    let unassignedHgNodes = List.filter (hgNodeAlreadyAssigned >> not) allHgNodes
                    let sgNodesToBeMatched = List.map (fun n -> (n, List.filter (fun n2 -> n2 <> n) sgNodesLeft)) sgNodesLeft
                    let possibleAssignmentPairings = List.allPairs sgNodesToBeMatched unassignedHgNodes
                    let folder currentMapping ((sgNodeToTry,sgNodesNowLeft), hgNodeToTry) =
                        match currentMapping with
                        | Some a -> a |> Some
                        | None ->
                            let mappingAttempt = Map.add sgNodeToTry hgNodeToTry mapping
                            let searchRes = search hg sg sgNodesNowLeft mappingAttempt
                            match searchRes with
                            | Some validAssign -> validAssign |> Some
                            | None -> None
                    List.fold folder None possibleAssignmentPairings
        let subGNodes = Graph.nodes subgraph |> List.map (fun n -> n.id)
        search hostGraph subgraph subGNodes Map.empty

    let sgiFirstWithEdge hostGraph subgraph =
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

        let rec search hg sg sgNodesLeft mapping =
            let mapNode (n : Node) = Map.find n.id mapping |> fun i -> Graph.node i hg
            let mapEdge (e : Edge) = {e with source = mapNode e.source; target = mapNode e.target; weight=None}
            let edgesInSg = Graph.edges sg
            let edgeInCurrentSg (e : Edge) = (Map.containsKey e.source.id mapping) && (Map.containsKey e.target.id mapping)
            let edgesInCurrentSg = List.filter edgeInCurrentSg edgesInSg
            let mappedEdgesInCurrentSg = List.map mapEdge edgesInCurrentSg
            let edgesInHg = Graph.edges hg |> List.map (fun e -> {e with weight=None}) |> set
            match Set.isSubset (set mappedEdgesInCurrentSg) edgesInHg with
            | false -> None
            | true -> 
                match sgNodesLeft with
                | [] ->
                    match edgesInSg = edgesInCurrentSg with
                    | false -> None
                    | true -> mapping |> Some
                | _ ->
                    let allHgNodes = hg |> Graph.nodes |> List.map (fun n -> n.id)
                    let hgNodeAlreadyAssigned id =
                        Map.tryFindKey (fun k v -> v=id) mapping
                        |> function
                        | Some _ -> true
                        | None -> false
                    let unassignedHgNodes = List.filter (hgNodeAlreadyAssigned >> not) allHgNodes
                    let sgNodesToBeMatched = List.map (fun n -> (n, List.filter (fun n2 -> n2 <> n) sgNodesLeft)) sgNodesLeft
                    let possibleAssignmentPairings = List.allPairs sgNodesToBeMatched unassignedHgNodes
                    let folder currentMapping ((sgNodeToTry,sgNodesNowLeft), hgNodeToTry) =
                        match currentMapping with
                        | Some a -> a |> Some
                        | None ->
                            let mappingAttempt = Map.add sgNodeToTry hgNodeToTry mapping
                            let searchRes = search hg sg sgNodesNowLeft mappingAttempt
                            match searchRes with
                            | Some validAssign -> validAssign |> Some
                            | None -> None
                    List.fold folder None possibleAssignmentPairings

        let subGNodes = subgraph |> Graph.nodes |> List.map (fun n -> n.id)
        let nMapRes = search hostGraph subgraph subGNodes Map.empty
        match nMapRes with
        | None -> None
        | Some nMap ->
            let subGEdges = subgraph |> Graph.idEdges |> List.map fst
            let eMapRes = edgeSearch hostGraph subgraph subGEdges nMap Map.empty
            match eMapRes with
            | None -> None
            | Some eMap -> (nMap,eMap) |> Some

    let sgiAllWithEdge hostGraph subgraph =
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

        let nMaps = sgiAll hostGraph subgraph |> Set.toList
        match nMaps with
        | [] -> []
        | _ ->
            let subGEdges = subgraph |> Graph.idEdges |> List.map fst
            let folder neMaps nMap =
                let eMapRes = edgeSearch hostGraph subgraph subGEdges nMap Map.empty
                match eMapRes with
                | None -> neMaps
                | Some eMap -> (nMap,eMap) :: neMaps
            List.fold folder [] nMaps            



    // Potential refactoring, though code is less readable here...
    // ===========================================================
    // let rec sgiSearch hg sg sgNodesLeft mapping f =
    //     let mapNode (n : Node) = Map.find n.id mapping |> Graph.node hg
    //     let mapEdge e = {e with source = mapNode e.source; target = mapNode e.target}
    //     let edgesInSg = Graph.edges sg
    //     let edgeInCurrentSg e = (Map.containsKey e.source.id mapping) && (Map.containsKey e.target.id mapping)
    //     let edgesInCurrentSg = List.filter edgeInCurrentSg edgesInSg
    //     let mappedEdgesInCurrentSg = List.map mapEdge edgesInCurrentSg
    //     let edgesInHg = Graph.edges hg |> set
    //     match Set.isSubset (set mappedEdgesInCurrentSg) edgesInHg with
    //     | false -> Set.empty
    //     | true -> 
    //         match sgNodesLeft with
    //         | [] ->
    //             match edgesInSg = edgesInCurrentSg with
    //             | false -> Set.empty
    //             | true -> Set.ofList [mapping]
    //         | _ ->
    //             let allHgNodes = hg |> Graph.nodes |> List.map (fun n -> n.id)
    //             let hgNodeAlreadyAssigned id =
    //                 Map.tryFindKey (fun k v -> v=id) mapping
    //                 |> function
    //                 | Some _ -> true
    //                 | None -> false
    //             let unassignedHgNodes = List.filter (hgNodeAlreadyAssigned >> not) allHgNodes
    //             let sgNodesToBeMatched = List.map (fun n -> (n, List.filter (fun n2 -> n2 <> n) sgNodesLeft)) sgNodesLeft
    //             let possibleAssignmentPairings = List.allPairs sgNodesToBeMatched unassignedHgNodes
    //             f hg sg mapping possibleAssignmentPairings                    

    // let sgiAll2 hostGraph subgraph =    
    //     let rec f hostGraph subgraph mapping (assignmentPairings : ((NodeId * NodeId list) * NodeId) list) =
    //         let folder validAssignments ((sgNodeToTry,sgNodesNowLeft), hgNodeToTry) =
    //             let mappingAttempt = Map.add sgNodeToTry hgNodeToTry mapping
    //             let searchRes = sgiSearch hostGraph subgraph sgNodesNowLeft mappingAttempt f
    //             searchRes + validAssignments
    //         List.fold folder Set.empty assignmentPairings
    //     let sgNodes = Graph.nodes subgraph |> List.map (fun n -> n.id)        
    //     sgiSearch hostGraph subgraph sgNodes Map.empty f