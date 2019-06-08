module Bilbo.Evaluator.PrimativeType

open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.Type
open Bilbo.Common.Result

let evalLiteral l : BilboResult<Meaning> =
    let lit =
        match l with
        | StrLit s -> s |> Value.String
        | FloatLit f -> f |> Float
        | IntLit i -> i |> Int
        | BoolLit b -> b |> Bool
    lit
    |> Value
    |> Ok

let typeConvert typ (v : Value) : BilboResult<Value> =
    match typ,v with
    | CastString, String x -> v |> Ok
    | CastString, Float x -> x |> string |> Value.String |> Ok
    | CastString, Int x -> x |> string |> Value.String |> Ok
    | CastString, Bool true -> "True" |> Value.String |> Ok
    | CastString, Bool false -> "False" |> Value.String |> Ok
    | CastFloat, String x ->
        match System.Double.TryParse x with
        | true, x' -> x' |> Float |> Ok
        | _ -> typeCastValueError "str" "float"
    | CastFloat, Float x -> v |> Ok
    | CastFloat, Int x -> x |> float |> Float |> Ok
    | CastFloat, Bool true -> 1.0 |> Float |> Ok
    | CastFloat, Bool false -> 0.0 |> Float |> Ok     
    | CastInt, String x ->
        match System.Int32.TryParse x with
        | true, x' -> x' |> Int |> Ok
        | _ -> typeCastValueError "str" "int"
    | CastInt, Float x -> x |> System.Math.Round |> int |> Int |> Ok
    | CastInt, Int x -> v |> Ok
    | CastInt, Bool true -> 1 |> Int |> Ok
    | CastInt, Bool false -> 0 |> Int |> Ok
    | CastBool, String x -> (x <> "") |> Bool |> Ok
    | CastBool, Float x -> (x <> 0.0) |> Bool |> Ok
    | CastBool, Int x -> (x <> 0) |> Bool |> Ok
    | CastBool, Bool x -> v |> Ok
    | CastString, _ -> typeCastValueError (v |> typeStrValue) "str"
    | CastFloat, _ -> typeCastValueError (v |> typeStrValue) "float"
    | CastInt, _ -> typeCastValueError (v |> typeStrValue) "int"
    | CastBool, _ -> typeCastValueError (v |> typeStrValue) "bool"