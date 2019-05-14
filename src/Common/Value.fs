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
    | Pipeline  of Pipeline
    | Node of Node
    | Graph of Graph
    // TODO: Pipelines with <|>

and Pipeline = PStage list
and ParamList = Meaning list

and PStage =
    | Transform of TransformDef * SymbolTable
    | Function of FunctionDef * SymbolTable
    // | ParamStage of ParamList


// Useful type definitions for readability
and NodeId = Meaning
and NodeLoad = Meaning
and EdgeWeight = Meaning option
and EdgeId = int
and EdgeMap<'T> = Map<NodeId,'T>
and NodeMap<'T> = Map<NodeId,'T>

and Node = {
    id   : NodeId
    load : NodeLoad
}

and Edge = {
    source  : Node
    weight  : EdgeWeight
    target  : Node
}

and Graph = {
    // node id -> node load
    nodes       : Map<NodeId, NodeLoad>
    // loads       : Map<NodeLoad, NodeId list>
    // source node id -> (target node id -> [edge weights])
    sourceEdges : EdgeMap<EdgeMap<(EdgeWeight*EdgeId) list>>
    // tagret node id -> (source node id -> [edge weights])
    targetEdges : EdgeMap<EdgeMap<(EdgeWeight*EdgeId) list>>
    edgeIdCount : EdgeId
}

and ValueId = {
    spLst   : SpaceId list
    id      : Id
}

and UnboundNodeId = Meaning
and UnboundEdgeWeight = Meaning option

and UnboundNode = {
    nid : UnboundNodeId
}

and UnboundEdge = {
    source  : UnboundNode
    weight  : UnboundEdgeWeight 
    target  : UnboundNode 
}

and UnboundGraph = {
    nodes   : Set<UnboundNode>
    edges   : (UnboundEdge*EdgeId) list
    edgeIdCount : EdgeId      
}