module Bilbo.Evaluator.PrefixExpression

open Bilbo.Common.Type
open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.Result
open Bilbo.Evaluator.PrimativeType

let notRules m : BilboResult<Meaning> =
    match m with
    | Value v ->
        let inv = typeConvert CastBool v
        match inv with
        | Error e -> e |> Error
        | Ok (Bool b) -> b |> not |> Bool |> Value |> Ok
        | Ok v' -> v' |> typeStrValue |> fun from -> typeCastValueError from "bool"
    | _ -> m |> typeStr |> fun from -> typeCastValueError from "bool"

let ampRules m : BilboResult<Meaning>=
    match m with
    | Value(Node n) -> n.id |> Ok
    | _ -> m |> typeStr |> nonNodeAmpExpr

let hashRules m : BilboResult<Meaning>=
    match m with
    | Value(Node n) -> n.load |> Ok
    | _ -> m |> typeStr |> nonNodeHashExpr


let dollarRules m =
    match m with
    | Value (Pipeline(pl)) -> (pl,Once) |> Modified |> Pipeline |> Value |> Ok
    | _ -> m |> typeStr |> nonPipelineDollarApp