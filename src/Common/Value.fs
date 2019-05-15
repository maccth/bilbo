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
    nodes   : Map<NodeId, NodeLoad>
    edges   : Map<EdgeId, Edge>
    edgeIdCount : EdgeId
}

and ValueId = {
    spLst   : SpaceId list
    id      : Id
}

and UnboundNodeId = NodeId
and UnboundEdgeId = EdgeId
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
    edges   : Set<UnboundEdgeId*UnboundEdge>
    edgeIdCount : EdgeId      
}