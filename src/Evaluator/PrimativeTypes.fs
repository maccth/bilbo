module Bilbo.Evaluator.PrimativeTypes

open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Common.Error

let typeConvert typ (v : Value) : BilboResult<Value> =
    match typ,v with
    | "str", String x -> v |> Ok
    | "str", Float x -> x |> string |> Value.String |> Ok
    | "str", Int x -> x |> string |> Value.String |> Ok
    | "str", Bool true -> "True" |> Value.String |> Ok
    | "str", Bool false -> "False" |> Value.String |> Ok
    | "float", String x ->
        match System.Double.TryParse x with
        | true, x' -> x' |> Float |> Ok
        | _ ->
            "Cannot convert value to float"
            |> ValueError
            |> Error
    | "float", Float x -> v |> Ok
    | "float", Int x -> x |> float |> Float |> Ok
    | "float", Bool true -> 1.0 |> Float |> Ok
    | "float", Bool false -> 0.0 |> Float |> Ok     
    | "int", String x ->
        match System.Int32.TryParse x with
        | true, x' -> x' |> Int |> Ok
        | _ ->
            "Cannot convert value to int"
            |> ValueError
            |> Error
    | "int", Float x -> x |> System.Math.Round |> int |> Int |> Ok
    | "int", Int x -> v |> Ok
    | "int", Bool true -> 1 |> Int |> Ok
    | "int", Bool false -> 0 |> Int |> Ok
    | "bool", String x -> (x <> "") |> Bool |> Ok
    | "bool", Float x -> (x <> 0.0) |> Bool |> Ok
    | "bool", Int x -> (x <> 0) |> Bool |> Ok
    | "bool", Bool x -> v |> Ok
    | _ ->
        "Cannot convert to a primative type"
        |> ValueError
        |> Error

let evalLiteral l : BilboResult<Meaning> =
    match l with
    | StrLit s -> s |> Value.String
    | FloatLit f -> f |> Float
    | IntLit i -> i |> Int
    | BoolLit b -> b |> Bool
    |> Value
    |> Ok