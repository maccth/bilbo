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
    // TODO: Graphs and paths
    // TODO: Pipelines with <|>

and Node = {
    id : Meaning
    load : Meaning
}

and Pipeline = PStage list

and ParamList = Meaning list

and PStage =
    | Transform of TransformDef * SymbolTable
    | Function of FunctionDef * SymbolTable
    // | ParamStage of ParamList

and Meaning =
    | Value of Value
    | Space of SpaceType * SymbolTable
    | ParamList of ParamList

and SpaceType =
    | Namespace
    | Object of TypeName

and SymbolTable = Map<Id, Meaning>