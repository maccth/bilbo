module Bilbo.Common.Type

open Bilbo.Common.Value

let rec typeStr (mean : Meaning) : string =
    match mean with
    | ParamList _ -> "parameter list"
    | Space (Namespace, _) -> "namespace"
    | Space (Object tName, _) -> "object (of type " + tName + ")"
    | Value v -> v |> typeStrValue

and typeStrValue (v : Value) : string =
    match v with
    | Type _ -> "type definition"
    | String _ -> "str"
    | Float _ -> "float"
    | Int _ ->  "int"
    | Bool _ -> "bool"
    | Pipeline _ -> "pipeline"
    | Node n ->
        let idT = typeStr n.id
        let loadT = typeStr n.load
        "node (with identifier of type " + idT + " and load of type " + loadT + ")"
    | Graph _ -> "graph"