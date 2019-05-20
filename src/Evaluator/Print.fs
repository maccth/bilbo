module Bilbo.Evaluator.Print

open Bilbo.Common.Extensions
open Bilbo.Common.Value
open Bilbo.Common.Error
open Bilbo.Common.Type
open Bilbo.Graph.Graph

let strFold join lst =
    let folder join prevPart elem  =
        match prevPart, elem with
        | Error e, _ -> e |> Error
        | _, Error e -> e |> Error
        | Ok s1, Ok s2 -> s1 + join + s2 |> Ok
    match lst with
    | [one] -> one
    | first :: rest -> List.fold (folder join) first rest
    | _ -> List.fold (folder join) (Ok "") lst

let rec print (mean : Meaning) : BilboResult<string> =
    match mean with
    | ParamList _ -> mean |> typeStr |> printError 
    | Space (Namespace, _) -> mean |> typeStr |> printError
    | Space (Object tName, st) -> objPrint tName st
    | Value v -> v |> valuePrint
 
and valuePrint (v : Value) =
    match v with
    | String s -> "\"" + s + "\"" |> Ok
    | Float f -> f |> string |> Ok 
    | Int i -> i |> string |> Ok
    | Bool b when b -> "True" |> Ok
    | Bool b -> "False" |> Ok
    | Pipeline _ -> v |> typeStrValue |> printError 
    | Node n -> nodePrint n
    | Type _ -> "Type definition printing" |> notImplementedYet
    | Graph g -> graphPrint g
    | Collection c -> collectionPrinting c

and nodePrint n =
    let id = print n.id
    match id with
    | Error e -> e |> Error
    | Ok idStr ->
        let load = print n.load
        match load with
        | Error e -> e |> Error
        | Ok loadStr -> idStr+"::"+loadStr |> Ok

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

and edgePrint (e : Edge) =
    let s = nodePrint e.source
    let t = nodePrint e.target
    let edgeW =
        match e.weight with
        | None -> ">" |> Ok
        | Some w ->
            w
            |> print
            |=> fun wStr -> wStr + ">"
    match s,edgeW,t with
    | Error e, _, _ 
    | _, Error e, _
    | _, _, Error e -> e |> Error
    | Ok s', Ok w', Ok t' ->
        "[" + s' + ", " + w' + ", " + t' + "]" |> Ok

and graphPrint (g : Graph) =
    // First print print the edges then the nodes that do not appear in any edges (as a single path)
    // So [a,>,b,c] should be printed as [a,>,b] + [c]
    // and [a,>,b,c,d] as [a,>,b] + [c,d]
    let plusFold = strFold " + "
    let commaFold = strFold ", "
    let edges = Graph.edges g |> List.sort
    let edgeStrs = edges |> List.map edgePrint
    let edgeStr = plusFold edgeStrs
    let endPoints = edges |> List.collect (fun e -> [e.source; e.target]) |> Set.ofList
    let nodeStrs = (Graph.nodes g |> set) - endPoints |> Set.toList |> List.sort |> List.map nodePrint
    let nodeStr =
        match nodeStrs with
        | [] -> "" |> Ok
        | _ -> nodeStrs |> commaFold |=> fun s -> "["+s+"]"
    match edgeStr, nodeStr with
    | Ok "", Ok "" -> "[]" |> Ok
    | Ok "", _ -> nodeStr
    | _, Ok "" -> edgeStr
    | _ -> plusFold [edgeStr; nodeStr]

and collectionPrinting (c : Collection) =
    let cStrs = List.map (Graph >> valuePrint) c
    strFold "\n|&| " cStrs