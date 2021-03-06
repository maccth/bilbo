module Bilbo.Common.Value

open Bilbo.Common.Ast

type SymbolTable = Map<Id, Meaning>

and Meaning =
    | Value of Value
    | Space of SpaceType * SymbolTable
    | ParamList of ParamList

and SpaceType =
    | Namespace
    | Object of TypeName

and Value =
    | Type of TypeDef
    | String of string
    | Float of float
    | Int of int
    | Bool of bool
    | Pipeline of Pipeline
    | Node of Node
    | Graph of Graph
    | Collection of Collection
    | Reducer of Reducer

and ValueId = {
    spLst   : SpaceId list
    id      : Id
}

and ParamList = Meaning list

and Pipeline =
    | PStage of PStage
    | Modified of Pipeline * Modifier
    | Reduction of Pipeline * Reducer
    | ThenPipe of Pipeline * Pipeline
    | OrPipe of Pipeline * Pipeline * Meaning list
    | AndPipe of Pipeline * Pipeline

and PStage =
    | Transform of TransformDef * SymbolTable
    | Function of FunctionDef * SymbolTable
    // | ParamStage of ParamList

and Modifier =
    | Alap
    | Maybe of Meaning option
    | Once

and Reducer =
    | Filter
    | Min
    | Max

and PipelineOutput<'T> =
    | Unfinished of 'T
    | Output of 'T

// Useful type definitions for readability
and NodeId = Meaning
and NodeLoad = Meaning
and EdgeWeight = Meaning option
and EdgeId = int

and Node = {
    id   : NodeId
    load : NodeLoad
}

and Edge = {
    source  : Node
    weight  : EdgeWeight
    target  : Node
}

and EdgeInfo = {
    source  : NodeId
    weight  : EdgeWeight
    target  : NodeId
    id      : EdgeId
}

and Graph = {
    nodes   : Map<NodeId, NodeLoad>
    edges   : Map<EdgeId, EdgeInfo>
    edgeIdCount : EdgeId
}

and UnboundNodeId = NodeId
and UnboundEdgeId = EdgeId
and UnboundEdgeWeight = EdgeWeight

and UnboundNode = {
    nid : UnboundNodeId
}

and UnboundEdge = {
    source  : UnboundNode
    weight  : UnboundEdgeWeight 
    target  : UnboundNode 
}

and UnboundEdgeInfo = {
    source  : UnboundNodeId
    weight  : UnboundEdgeWeight
    target  : UnboundNodeId
    id      : EdgeId
}

and UnboundGraph = {
    nodes   : Set<UnboundNode>
    edges   : Map<UnboundEdgeId,UnboundEdgeInfo>
    edgeIdCount : EdgeId      
}

and NodeMap = Map<UnboundNodeId,NodeId>
and EdgeMap = Map<UnboundEdgeId,EdgeId>

// For  [+] and [-]
// and SpecialValues = {
//     posPatternGraph : Graph option
//     negPatternGraph : Graph option
// }

and Collection = Set<Graph>