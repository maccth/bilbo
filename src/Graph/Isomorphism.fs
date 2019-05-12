module Bilbo.Graph.Isomorphism

open Bilbo.Common.Value
open Bilbo.Common.Error
open Bilbo.Common.SymbolTable
open Bilbo.Graph.Graph

let rec search graph subgraph (subgraphNodesLeft : NodeId list) (assignments : Map<NodeId,NodeId>) =
    match subgraphNodesLeft with
    | [] -> assignments |> Some
    | subgraphNode :: rest ->
        let possibleHostNodeAssignments = graph |> Graph.nodes |> List.map (fun n -> n.id)
        let hostGraphNodeAlreadyAssigned id =
            Map.tryFindKey (fun k v -> v=id) assignments
            |> function
            | Some _ -> true
            | None -> false
        let refinedHostNodePosAssignments = List.filter (hostGraphNodeAlreadyAssigned >> not) possibleHostNodeAssignments
        let folder assignResults (hostNodeToTry) =
            let assignments' = Map.add subgraphNode hostNodeToTry assignments
            let searchRes = search graph subgraph rest assignments'
            match searchRes with
            | Some assignGood -> assignGood :: assignResults
            | None -> assignResults
        let goodMatches = List.fold folder [] refinedHostNodePosAssignments
        match goodMatches with
        | [] -> None
        // For all subgraph isomorphisms, the one::rest here
        | one :: rest -> one |> Some

let isomorphism g subG =
    search g subG (Graph.nodes subG |> List.map (fun n -> n.id)) Map.empty








