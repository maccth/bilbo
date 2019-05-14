module Bilbo.Graph.Isomorphism

open Bilbo.Common.Value
open Bilbo.Common.Error
open Bilbo.Common.SymbolTable
open Bilbo.Graph.Graph

// TODO:
// [ ] Check edges, becuase we aren't just doing node induced subgraphs
// [x] Return complete list of subgraph isomorphisms
// [ ] Create basic sgi -> bool function so can do negative application conditions

module Graph =

    let sgiAll hostGraph subgraph =
        let rec search hg sg sgNodesLeft mapping =
            let mapNode (n : Node) = Map.find n.id mapping |> fun i -> Graph.node i hg
            let mapEdge (e : Edge) = {e with source = mapNode e.source; target = mapNode e.target}
            let edgesInSg = Graph.edges sg
            let edgeInCurrentSg (e : Edge) = (Map.containsKey e.source.id mapping) && (Map.containsKey e.target.id mapping)
            let edgesInCurrentSg = List.filter edgeInCurrentSg edgesInSg
            let mappedEdgesInCurrentSg = List.map mapEdge edgesInCurrentSg
            let edgesInHg = Graph.edges hg |> set
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
                    let hgNodeAlreadyAssigned id =
                        Map.tryFindKey (fun k v -> v=id) mapping
                        |> function
                        | Some _ -> true
                        | None -> false
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
            let mapEdge (e : Edge) = {e with source = mapNode e.source; target = mapNode e.target}
            let edgesInSg = Graph.edges sg
            let edgeInCurrentSg (e : Edge) = (Map.containsKey e.source.id mapping) && (Map.containsKey e.target.id mapping)
            let edgesInCurrentSg = List.filter edgeInCurrentSg edgesInSg
            let mappedEdgesInCurrentSg = List.map mapEdge edgesInCurrentSg
            let edgesInHg = Graph.edges hg |> set
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
        let subGNodes = Graph.nodes subgraph |> List.map (fun n -> n.id)
        search hostGraph subgraph subGNodes Map.empty

    // let sgiFirstEdge hostGraph subgraph =
    //     // nMapping : NodeId in subgraph -> NodeId in  hostGraph
    //     // eMapping : (source NodeId, target NodeId, index in edge weight list) -> index in host graph edge weight list
    //     let rec search hg sg sgNodesLeft (nMapping : Map<NodeId,NodeId>) (eMapping : Map<EdgeId,EdgeId>) =
    //         let mapNode (n : Node) = Map.find n.id nMapping |> fun i -> Graph.node i hg
    //         let mapEdge (e : Edge) = {e with source = mapNode e.source; target = mapNode e.target}
    //         let edgesInSg = Graph.idEdges sg
    //         let edgeInCurrentSg (e : Edge) = (Map.containsKey e.source.id nMapping) && (Map.containsKey e.target.id nMapping)
    //         let edgesInCurrentSg = List.filter edgeInCurrentSg edgesInSg
    //         let mappedEdgesInCurrentSg = List.map mapEdge edgesInCurrentSg
    //         let edgesInHg = Graph.edges hg |> set
    //         match Set.isSubset (set mappedEdgesInCurrentSg) edgesInHg with
    //         | false -> None
    //         | true -> 
    //             match sgNodesLeft with
    //             | [] ->
    //                 match edgesInSg = edgesInCurrentSg with
    //                 | false -> None
    //                 | true -> nMapping |> Some
    //             | _ ->
    //                 let allHgNodes = hg |> Graph.nodes |> List.map (fun n -> n.id)
    //                 let hgNodeAlreadyAssigned id =
    //                     Map.tryFindKey (fun k v -> v=id) nMapping
    //                     |> function
    //                     | Some _ -> true
    //                     | None -> false
    //                 let unassignedHgNodes = List.filter (hgNodeAlreadyAssigned >> not) allHgNodes
    //                 let sgNodesToBeMatched = List.map (fun n -> (n, List.filter (fun n2 -> n2 <> n) sgNodesLeft)) sgNodesLeft
    //                 let possibleAssignmentPairings = List.allPairs sgNodesToBeMatched unassignedHgNodes
    //                 let folder currentMapping ((sgNodeToTry,sgNodesNowLeft), hgNodeToTry) =
    //                     match currentMapping with
    //                     | Some a -> a |> Some
    //                     | None ->
    //                         let mappingAttempt = Map.add sgNodeToTry hgNodeToTry nMapping
    //                         let searchRes = search hg sg sgNodesNowLeft mappingAttempt eMapping
    //                         match searchRes with
    //                         | Some validAssign -> validAssign |> Some
    //                         | None -> None
    //                 List.fold folder None possibleAssignmentPairings
    //     let subGNodes = Graph.nodes subgraph |> List.map (fun n -> n.id)
    //     search hostGraph subgraph subGNodes Map.empty

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