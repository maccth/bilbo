module Bilbo.Evaluator.BinaryExpressions

open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Common.Error
open System
open Bilbo.Common

let (<.|.>) first (second : Lazy<'T option>) =
    match first with
    | Some s -> s |> Some
    | None ->
        match second.Force() with
        | Some s -> s |> Some
        | None -> None

let noneReplace rep x =
    match x with
    | Some x' -> x'
    | None -> rep

let intFloat ops iiFun ifFun fiFun ffFun erFun =
    match ops with
    | Error e -> e |> Error |> Some
    | Ok (Value lhs', Value rhs') ->
        match lhs',rhs' with
        | Int x, Int y -> (iiFun x y) |> Value |> Ok |> Some
        | Int x, Float y -> (ifFun x y) |> Value |> Ok |> Some
        | Float x, Int y -> (fiFun x y) |> Value |> Ok |> Some
        | Float x, Float y -> (ffFun x y) |> Value |> Ok |> Some
        // TODO: consider changing to None and only adding error
        // at the end. Will allow easier composition of matches
        | _ -> erFun |> Error |> Some
    | _ -> None

let intFloatStr ops iiFun ifFun fiFun ffFun ssFun erFun =
    let str = lazy(
        match ops with
        | Error e -> e |> Error |> Some
        | Ok (Value lhs', Value rhs') ->
            match lhs',rhs' with
            | String x, String y -> (ssFun x y) |> Value |> Ok |> Some
            | _ -> erFun |> Error |> Some
        | _ -> None)        
    (intFloat ops iiFun ifFun fiFun ffFun erFun) <.|.> str
        
let intFloat2 ops iiFun ifFun fiFun ffFun erFun =
    let iiFun2 x y = (iiFun x y) |> Int
    let ifFun2 x y = (ifFun x y) |> Float
    let fiFun2 x y = (fiFun x y) |> Float
    let ffFun2 x y = (ffFun x y) |> Float
    intFloat ops iiFun2 ifFun2 fiFun2 ffFun2 erFun

let intFloatStr2 ops iiFun ifFun fiFun ffFun ssFun erFun =
    let ssFun2 x y = (ssFun x y) |> Value.String
    let str2 =
        match ops with
        | Error e -> e |> Error |> Some
        | Ok (Value lhs', Value rhs') ->
            match lhs',rhs' with
            | String x, String y -> (ssFun2 x y) |> Value |> Ok |> Some
            | _ -> erFun |> Error |> Some
        | _ -> None        
    intFloat2 ops iiFun ifFun fiFun ffFun erFun

let operatorErr = 
    // TODO: Implement!
    "Operator error."
    |> ImplementationError

let plusRules (ops : BilboResult<Meaning * Meaning>) : BilboResult<Meaning> =
    intFloatStr2 ops (+) (fun x y -> float(x) + y) (fun x y -> x + float(y)) (+) (+) operatorErr
    |> noneReplace ("Not implemented yet." |> ImplementationError |> Error)
    // TODO: Add graph-based plus operations

let minusRules ops =
    intFloat2 ops (-) (fun x y -> float(x) - y) (fun x y -> x - float(y)) (-) operatorErr
    |> noneReplace ("Not implemented yet." |> ImplementationError |> Error)
    // TODO: Add graph-based minus operations

let timesRules ops =
    intFloat2 ops (*) (fun x y -> float(x) * y) (fun x y -> x * float(y)) (*) operatorErr
    |> noneReplace ("Not implemented yet." |> ImplementationError |> Error)

let zeroCheck ops =
    match ops with
    | Error e -> e |> Error |> Some
    | Ok (Value lhs', Value rhs') ->
        match lhs',rhs' with
        | (_, Int y) when y=0 ->
            "Cannot divide or modulo by zero" |> ValueError |> Error |> Some
        | (_, Float y) when y=0.0 ->
            "Cannot divide or modulo by zero" |> ValueError |> Error |> Some
        | _ -> None
    | _ -> None  

let divideRules ops =
    let z = zeroCheck ops
    let eval = lazy(intFloat2 ops (/) (fun x y -> float(x) / y) (fun x y -> x / float(y)) (/) operatorErr)
    z <.|.> eval
    |> noneReplace ("Not implemented yet." |> ImplementationError |> Error)
    
let moduloRules ops =
    let z = zeroCheck ops
    let eval = lazy(intFloat2 ops (%) (fun x y -> float(x) % y) (fun x y -> x % float(y)) (%) operatorErr)
    z <.|.> eval
    |> noneReplace ("Not implemented yet." |> ImplementationError |> Error)

let powRules ops =
    let iiFun = fun x y -> float(x) ** float(y) |> Float
    let ifFun = fun x y -> float(x) ** y |> Float
    let fiFun = fun x y -> x ** float(y) |> Float    
    let ffFun = fun x y -> x ** y |> Float    
    intFloat ops iiFun ifFun fiFun ffFun operatorErr
    |> noneReplace ("Not implemented yet." |> ImplementationError |> Error)

let ltRules ops =
    let lt = fun x y -> (x < y) |> Bool
    let ifFun = fun x y -> (float(x) < y) |> Bool
    let fiFun = fun x y -> x < float(y) |> Bool
    intFloat ops lt ifFun fiFun lt operatorErr
    |> noneReplace ("Not implemented yet." |> ImplementationError |> Error)

let lteqRules ops =
    let lteq = fun x y -> (x <= y) |> Bool
    let ifFun = fun x y -> (float(x) <= y) |> Bool
    let fiFun = fun x y -> x <= float(y) |> Bool
    intFloat ops lteq ifFun fiFun lteq operatorErr
    |> noneReplace ("Not implemented yet." |> ImplementationError |> Error)

let gtRules ops =
    // Unpacking the result of lteqRules and inverting it would take as many line...
    let gt = fun x y -> (x > y) |> Bool
    let ifFun = fun x y -> (float(x) > y) |> Bool
    let fiFun = fun x y -> x > float(y) |> Bool
    intFloat ops gt ifFun fiFun gt operatorErr
    |> noneReplace ("Not implemented yet." |> ImplementationError |> Error)

let gteqRules ops =
    let gteq = fun x y -> (x >= y) |> Bool
    let ifFun = fun x y -> (float(x) >= y) |> Bool
    let fiFun = fun x y -> x >= float(y) |> Bool
    intFloat ops gteq ifFun fiFun gteq operatorErr
    |> noneReplace ("Not implemented yet." |> ImplementationError |> Error)

let equalsRules ops =
    // TODO: Add equality for non-primative (structural types) types
    let eq = fun x y -> (x = y) |> Bool
    let ifFun = fun x y -> (float(x) = y) |> Bool
    let fiFun = fun x y -> x = float(y) |> Bool
    intFloat ops eq ifFun fiFun eq operatorErr
    |> noneReplace ("Not implemented yet." |> ImplementationError |> Error)

let notEqualsRules ops =
    // TODO: Add non-equality for non-primative (structural types) types
    let neq = fun x y -> (x <> y) |> Bool
    let ifFun = fun x y -> (float(x) <> y) |> Bool
    let fiFun = fun x y -> x <> float(y) |> Bool
    intFloat ops neq ifFun fiFun neq operatorErr
    |> noneReplace ("Not implemented yet." |> ImplementationError |> Error)

let boolean ops binOp =
    let booleanTrue (v : Value) =
        // TODO: Add graph pattern matching `and` and `or` operartions for pos and neg graph
        // and non-primative types 
        match v with
        | Int x -> x<>0 |> Some
        | Float x -> x<>0.0 |> Some
        | String x -> x<>"" |> Some
        | Bool x -> x |> Some
        | _ -> None
    match ops with
    | Error e -> e |> Error |> Some
    | Ok (Value lhs, Value rhs) ->
        let lhs' = booleanTrue lhs
        let rhs' = booleanTrue rhs
        match lhs',rhs' with
        | Some l', Some r' -> binOp l' r' |> Some
        | _ -> None
    | _ -> None

let andRules (ops : BilboResult<Meaning*Meaning>) =
    boolean ops (fun x y -> (x && y) |> Bool |> Value |> Ok)
    |> noneReplace ("Not implemented yet." |> ImplementationError |> Error)

let orRules (ops : BilboResult<Meaning*Meaning>) =
    boolean ops (fun x y -> (x || y) |> Bool |> Value |> Ok)
    |> noneReplace ("Not implemented yet." |> ImplementationError |> Error)

let xorRules (ops : BilboResult<Meaning*Meaning>) =
    let xor x y = (x && (not y)) || ((not x) && y)
    boolean ops (fun x y -> (xor x y) |> Bool |> Value |> Ok)
    |> noneReplace ("Not implemented yet." |> ImplementationError |> Error)


(**
let plusRules (ops : BilboResult<Meaning * Meaning>) =
    match ops with
    | Ok (Value lhs', Value rhs') ->
        match lhs',rhs' with
        | Int x, Int y -> x + y |> Int |> Value |> Ok
        | Int x, Float y -> (float x) + y |> Float |> Value |> Ok
        | Float x, Int y -> x + (float y) |> Float |> Value |> Ok
        | Float x, Float y -> x + y |> Float |> Value |> Ok
        | String x, String y -> x + y |> String |> Value |> Ok
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
*)