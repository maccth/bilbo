module Bilbo.Common.Value

open Bilbo.Common.Ast

type Value =
    | Transform of TransformDef
    | Type of TypeDef
    | String of string
    | Float of float
    | Int of int
    | Bool of bool
    // | Node of Node
    // TODO: Graphs and paths
    // TODO: Pipelines with <|>
    // | Pipeline of TransformDef list
   

and Node = {
    id : Value
    load : Value
}