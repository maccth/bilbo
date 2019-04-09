module Bilbo.SemanticAnalyser.Value

type Value =
    | String of string
    | Float of float
    | Int of int
    | Bool of bool