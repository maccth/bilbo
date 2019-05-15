module Bilbo.Evaluator.Print

open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Common.Error
open Bilbo.Common.Type
open Bilbo.Graph.Graph

let rec print (mean : Meaning) : BilboResult<string> =
    match mean with
    | ParamList _ -> mean |> typeStr |> printError 
    | Space (Namespace, _) -> mean |> typeStr |> printError
    | Space (Object tName, st) -> objPrint tName st
    | Value v ->
        match v with
        | String s -> "\"" + s + "\"" |> Ok
        | Float f -> f |> string |> Ok 
        | Int i -> i |> string |> Ok
        | Bool b when b -> "True" |> Ok
        | Bool b -> "False" |> Ok
        | Pipeline _ -> mean |> typeStr |> printError 
        | Node n -> nodePrint n
        | Graph _ -> "Graph printing" |> notImplementedYet
        | Type _ -> "Type definition printing" |> notImplementedYet
 
and nodePrint n =
    let id = print n.id
    match id with
    | Error e -> e |> Error
    | Ok idStr ->
        let load = print n.load
        match load with
        | Error e -> e |> Error
        | Ok loadStr -> "("+idStr+")::("+loadStr+")" |> Ok

and objPrint tName (st : SymbolTable) =
    let printParamValPair id mean =
        match print mean with
        | Error e -> e |> Error
        | Ok meanStr -> id + "=" + meanStr + "" |> Ok
    let attrs = Map.toList st
    let folder str (id,mean) =
        match str with
        | Error e -> e |> Error
        | Ok str' ->
            let pairStr = printParamValPair id mean
            Result.bind (fun s -> ", " + s |> Ok) pairStr
    match attrs with
    | [] -> tName + "()" |> Ok
    | [(id,mean)] -> printParamValPair id mean |> Result.bind (fun s -> tName+"("+s+")" |> Ok)
    | (id,mean) :: rest ->
        let param1 = printParamValPair id mean
        let paramRest = List.fold folder (Ok "") attrs
        match param1, paramRest with
        | Ok s, Ok s2 -> tName + "(" + s + s2 + ")" |> Ok
        | Error e, _ -> e |> Error
        | _, Error e -> e |> Error