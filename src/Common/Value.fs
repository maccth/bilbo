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
    // TODO: Graphs and paths
    // TODO: Pipelines with <|>

and Pipeline = PStage list
and ParamList = Meaning list

and PStage =
    | Transform of TransformDef * SymbolTable
    | Function of FunctionDef * SymbolTable
    // | ParamStage of ParamList


// Useful type definitions for readability
and EdgeWeight = Meaning option
and NodeId = Meaning
and NodeLoad = Meaning

and Node = {
    id : NodeId
    load : NodeLoad
}


and Graph = {
    // node id -> node load
    nodes       : Map<NodeId,  NodeLoad>
    // source node id -> (target node id -> [edge weights])
    sourceEdges : Map<NodeId , Map<NodeId , EdgeWeight list> >
    // tagret node id -> (source node id -> [edge weights])
    targetEdges : Map<NodeId , Map<NodeId , EdgeWeight list> >
}