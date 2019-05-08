module Bilbo.Common.Value

open Bilbo.Common.Ast

type Value =
    | Pipeline of Pipeline
    | Type of TypeDef
    | String of string
    | Float of float
    | Int of int
    | Bool of bool
    | Node of Node
    // | Unit
    // TODO: Graphs and paths
    // TODO: Pipelines with <|>
    // | Pipeline of TransformDef list

and Pipeline = PStage list

and PStage =
    | Transform of TransformDef * SymbolTable
    | Function of FunctionDef * SymbolTable

and Node = {
    id : Meaning
    load : Meaning
}

and Meaning =
    | Value of Value
    | ParamList of Meaning list
    | Space of SpaceType * SymbolTable

and SpaceType =
    | Namespace
    | Object of TypeName

and SymbolTable = Map<Id, Meaning>