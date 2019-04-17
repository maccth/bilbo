module Bilbo.Evaluator.ExpressionStatement

open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Common.Error
open System 
open Bilbo.Common
open Bilbo.Common.SymbolTable

// Serious code sketch...
let plusRules x y =
    match x,y with
    | Int x', Int y' -> x' + y' |> Int |> Value |> Ok
    | Int x', Float y' -> (float x') + y' |> Float |> Value |> Ok
    | Float x', Int y' -> x' + (float y') |> Float |> Value |> Ok
    | String x', String y' -> x' + y' |> Value.String |> Value |> Ok
    | _ ->
        // TODO: Implement!
        "Operator error."
        |> ImplementationError
        |> Error

// Serious code sketch...
let (!*!) func (x : BilboResult<Meaning>) (y : BilboResult<Meaning>) =
    match x,y with
    | Ok (Value x'), Ok (Value y') -> func x' y'
    | Error x',_ -> x' |> Error     
    | _, Error y' -> y' |> Error

let evalLiteral l : BilboResult<Meaning> =
    match l with
    | StrLit s -> s |> Value.String
    | FloatLit f -> f |> Float
    | IntLit i -> i |> Int
    | BoolLit b -> b |> Bool
    |> Value
    |> Ok

let evalBinExpr op : BilboResult<'A -> 'B -> 'C> =
    match op with
    | Plus -> plusRules |> Ok
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error

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

let rec typeCast syms nLst typ attrs =
    match attrs with
    | [e] ->
        let e' = evalExpr syms nLst e
        match e' with
        | Ok (Value v) ->
            typeConvert typ v
            |> function
            | Ok v' -> v' |> Value |> Ok
            | Error v' -> v' |> Error
        | Ok _v ->
            "Cannot convert to a primative type"
            |> ValueError
            |> Error
        | Error err -> err |> Error
    | _ ->
        "Cannot convert to a primative type"
        |> ValueError
        |> Error    
    
and evalObjExpr syms nLst e : BilboResult<Meaning> =
    match e with
    | ObjInstan(typ, attrs) ->
        match typ with
        | "str"
        | "float"
        | "int"
        | "bool" -> typeCast syms nLst typ attrs


and evalExpr (syms : Symbols) nLst e : BilboResult<Meaning> =
    match e with
    | Var v -> Symbols.find syms {nLst=nLst; oLst=[]; id=v}
    | SExpr(Literal l) -> evalLiteral l
    | SExpr(ObjExpr o) -> evalObjExpr syms nLst o
    | BinExpr (lhs,op,rhs) ->
        let l = evalExpr syms nLst lhs
        let r = evalExpr syms nLst rhs
        match evalBinExpr op with
        | Ok op' -> !*!op' l r
        | Error e -> e |> Error
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error

let consVid syms nLst e : BilboResult<ValueId> =
    match e with
    |Var v -> {nLst=nLst; oLst=[]; id=v} |> Ok
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error
    
let evalExprStatement (syms : Symbols) nLst (e : ExprStatement) : BilboResult<Symbols> =
    match e with
    | AssignmentExpr (eLhs, eRhs) ->
        let lhsId = consVid syms nLst eLhs
        match lhsId with
        | Error e -> e |> Error
        | Ok vid ->
            let rhsVal = evalExpr syms nLst eRhs
            match rhsVal with
            | Error e -> e |> Error
            | Ok rhs ->
                Symbols.set syms vid rhs
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error