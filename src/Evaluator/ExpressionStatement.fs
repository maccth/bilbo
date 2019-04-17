module Bilbo.Evaluator.ExpressionStatement

open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Common.Error
open Bilbo.Evaluator.PrimativeTypes

// Serious code sketch...
let plusRules ops =
    match ops with
    | Ok (Value lhs', Value rhs') ->
        match lhs',rhs' with
        | Int x', Int y' -> x' + y' |> Int |> Value |> Ok
        | Int x', Float y' -> (float x') + y' |> Float |> Value |> Ok
        | Float x', Int y' -> x' + (float y') |> Float |> Value |> Ok
        | String x', String y' -> x' + y' |> Value.String |> Value |> Ok
        | _ ->
            // TODO: Implement!
            "Operator error."
            |> ImplementationError
            |> Error
    | Error e -> e |> Error
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error

let rec evalBinOperands syms spLst lhs rhs =
    let valL = evalExpr syms spLst lhs
    match valL with
    | Error e -> e |> Error
    | Ok valL' ->
        let valR = evalExpr syms spLst rhs
        match valR with
        | Ok valR' -> (valL', valR') |> Ok
        | Error e -> e |> Error

and (|..>) (syms,spLst,lhs,rhs) opRule =
    evalBinOperands syms spLst lhs rhs
    |> opRule

and evalBinExpr syms spLst lhs op rhs =
    match op with
    | Plus -> (syms,spLst,lhs,rhs) |..> plusRules
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error

and typeCast syms spLst typ attrs =
    match attrs with
    | [e] ->
        let e' = evalExpr syms spLst e
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
    
and evalObjExpr syms spLst e : BilboResult<Meaning> =
    match e with
    | ObjInstan(typ, attrs) ->
        match typ with
        | "str"
        | "float"
        | "int"
        | "bool" -> typeCast syms spLst typ attrs

and evalExpr (syms : Symbols) spLst e : BilboResult<Meaning> =
    match e with
    | Var v -> Symbols.find syms {spLst=spLst; id=v}
    | SExpr(Literal l) -> evalLiteral l
    | SExpr(ObjExpr o) -> evalObjExpr syms spLst o
    | BinExpr (lhs,op,rhs) -> evalBinExpr syms spLst lhs op rhs 
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error

let consVid syms spLst e : BilboResult<ValueId> =
    match e with
    | Var v -> {spLst=spLst; id=v} |> Ok
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error
    
let evalExprStatement (syms : Symbols) spLst (e : ExprStatement) : BilboResult<Symbols> =
    match e with
    | AssignmentExpr (eLhs, eRhs) ->
        let lhsId = consVid syms spLst eLhs
        match lhsId with
        | Error e -> e |> Error
        | Ok vid ->
            let rhsVal = evalExpr syms spLst eRhs
            match rhsVal with
            | Error e -> e |> Error
            | Ok rhs ->
                Symbols.set syms vid rhs
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error