module Bilbo.Evaluator.Evaluator

open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Common.Error

// Serious code sketch...
let plusRules x y =
    match x,y with
    | Int x', Int y' -> x' + y' |> Int |> SymbolTable.Value |> Ok
    | Int x', Float y' -> (float x') + y' |> Float |> SymbolTable.Value |> Ok
    | Float x', Int y' -> x' + (float y') |> Float |> SymbolTable.Value |> Ok
    | String x', String y' -> x' + y' |> String |> SymbolTable.Value |> Ok
    | _ ->
        // TODO: Implement!
        "Operator error."
        |> ImplementationError
        |> Error

// Serious code sketch...
let (!*!) func (x : BilboResult<SymbolTable.Meaning>) (y : BilboResult<SymbolTable.Meaning>) =
    match x,y with
    | Ok (SymbolTable.Value x'), Ok (SymbolTable.Value y') -> func x' y'
    | Error x',_ -> x' |> Error     
    | _, Error y' -> y' |> Error

let evalLiteral l : BilboResult<SymbolTable.Meaning> =
    match l with
    | StrLit s -> s |> String
    | FloatLit f -> f |> Float
    | IntLit i -> i |> Int
    | BoolLit b -> b |> Bool
    |> SymbolTable.Value
    |> Ok

let evalBinExpr op : BilboResult<'A -> 'B -> 'C> =
    match op with
    | Plus -> plusRules |> Ok
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error

let rec evalExpr (syms : Symbols.Bindings) nLst e : BilboResult<SymbolTable.Meaning> =
    match e with
    | Var v -> Symbols.find syms {nLst=nLst; oLst=[]; id=v}
    | SExpr(Literal l) -> evalLiteral l
    // Serious code sketch...
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
    
let evalExprStatement (syms : Symbols.Bindings) nLst (e : ExprStatement) : BilboResult<Symbols.Bindings> =
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



let evalProgramUnit (syms : Symbols.Bindings) pUnit : BilboResult<Symbols.Bindings> =
    let nLst,s = pUnit
    let rnLst = List.rev nLst
    match s with
    // | TypeDefL (loc,def) ->
    //     let tname = fst def
    //     let syms' = SymbolTable.addType syms rnLst tname def
    //     syms'
    //     |> Ok
    | ExprStatementL (loc, e) ->
        evalExprStatement syms rnLst e
    | ImportL (loc, i) ->
        syms |> Ok    
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error

let bilboEvaluate (ast : BilboResult<Program>) : BilboResult<Symbols.Bindings> =
    let rec evalRec syms ast  =
        match ast with
        | pUnit :: rest ->
           let syms' = evalProgramUnit syms pUnit
           match syms' with
           | Ok syms'' ->
                evalRec syms'' rest
            | Error e ->
                e |> Error
        | [] ->
            syms |> Ok
    match ast with
    | Ok ast' ->
        evalRec Symbols.empty ast'
    | Error err ->
       err |> Error

let bilboEvaluatorPrint (ast : BilboResult<Program>) =
    ast
    |> bilboEvaluate
    |> printfn "%A"