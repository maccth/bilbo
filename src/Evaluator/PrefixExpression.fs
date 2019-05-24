module Bilbo.Evaluator.PrefixExpression

open Bilbo.Common.Extensions
open Bilbo.Common.Type
open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.Error
open Bilbo.Graph.Graph
open Bilbo.Evaluator.Print
open Bilbo.Evaluator.PrimativeTypes
open Bilbo.Common

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

let dblAmpRules m : BilboResult<Meaning>=
    match m with
    | Value(Node n) -> n.load |> Ok
    | _ -> m |> typeStr |> nonNodeDblAmpExpr

