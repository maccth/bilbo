module Bilbo.SemanticAnalyser.Value

open Bilbo.Common.Ast

type Value =
    // For identifers whose value is to be determined at runtime
    | Unknown
    | String of string
    | Float of float
    | Int of int
    | Bool of bool
    | Object of TypeName * Map<string,Value>