module Bilbo.Evaluator.Print

open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Common.Error
open Bilbo.Common.Type
open Bilbo.Graph.Graph

let rec print (mean : Meaning) : BilboResult<string> =
    match mean with
    | ParamList _ -> mean |> typeStr |> printError 
    | Space (Namespace, _) -> mean |> typeStr |> printError
    | Value v ->
        match v with
        | String s -> s |> Ok
        | Float f -> f |> string |> Ok 
        | Int i -> i |> string |> Ok
        | Bool b when b -> "True" |> Ok
        | Bool b -> "False" |> Ok
        | Pipeline _ -> mean |> typeStr |> printError 
        | Node n -> nodePrint n    
        | Graph _ -> "Graph printing" |> notImplementedYet
        | Type _ -> "Type definition printing" |> notImplementedYet
    | Space (Object tName, _) -> "Object printing" |> notImplementedYet
 
and nodePrint n =
    let id = print n.id
    match id with
    | Error e -> e |> Error
    | Ok idStr ->
        let load = print n.load
        match load with
        | Error e -> e |> Error
        | Ok loadStr -> "("+idStr+")::("+loadStr+")" |> Ok