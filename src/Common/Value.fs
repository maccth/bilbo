module Bilbo.Common.Value

open Bilbo.Common.Ast

type Value =
    | Type of TypeDef
    | String of string
    | Float of float
    | Int of int
    | Bool of bool
    | Node of Node

    | Pipeline of Pipeline
    // | Unit
    // TODO: Graphs and paths
    // TODO: Pipelines with <|>

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
    | Space of SpaceType * SymbolTable
    | ParamList of Meaning list

and SpaceType =
    | Namespace
    | Object of TypeName

and SymbolTable = Map<Id, Meaning>