module Bilbo.Evaluator.BinaryExpressions

open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Common.Error
open Bilbo.Common.Ast
open Bilbo.Common

type Match<'T> =
    | Matched of 'T
    | NoMatch

let (<.|.>) (first : Match<'T>) (second : Lazy<Match<'T>>) =
    match first with
    | Matched s -> s |> Matched
    | NoMatch ->
        match second.Force() with
        | Matched s -> s |> Matched
        | NoMatch -> NoMatch

module Match =
    let underlie instead m =
        match m with
        | Matched m' -> m'
        | NoMatch -> instead

    let compose first second =
        match first with
        | Matched x -> x |> Matched
        | NoMatch ->
            match second with
            | Matched y -> y |> Matched
            | NoMatch -> NoMatch

    let lcompose first second =
        first <.|.> second

let intFloat ops iiFun ifFun fiFun ffFun =
    match ops with
    | Error e -> e |> Error |> Matched
    | Ok (Value lhs', Value rhs') ->
        match lhs',rhs' with
        | Int x, Int y -> (iiFun x y) |> Value |> Ok |> Matched
        | Int x, Float y -> (ifFun x y) |> Value |> Ok |> Matched
        | Float x, Int y -> (fiFun x y) |> Value |> Ok |> Matched
        | Float x, Float y -> (ffFun x y) |> Value |> Ok |> Matched
        | _ -> NoMatch
    | _ -> NoMatch

let intFloatStr ops iiFun ifFun fiFun ffFun ssFun =
    let str = lazy(
        match ops with
        | Error e -> e |> Error |> Matched
        | Ok (Value lhs', Value rhs') ->
            match lhs',rhs' with
            | String x, String y -> (ssFun x y) |> Value |> Ok |> Matched
            | _ -> NoMatch
        | _ -> NoMatch)        
    (intFloat ops iiFun ifFun fiFun ffFun) <.|.> str
        
let intFloat2 ops iiFun ifFun fiFun ffFun =
    let iiFun2 x y = (iiFun x y) |> Int
    let ifFun2 x y = (ifFun x y) |> Float
    let fiFun2 x y = (fiFun x y) |> Float
    let ffFun2 x y = (ffFun x y) |> Float
    intFloat ops iiFun2 ifFun2 fiFun2 ffFun2

let intFloatStr2 ops iiFun ifFun fiFun ffFun ssFun =
    let ssFun2 x y = (ssFun x y) |> Value.String
    let str2 = lazy(
        match ops with
        | Error e -> e |> Error |> Matched
        | Ok (Value lhs', Value rhs') ->
            match lhs',rhs' with
            | String x, String y -> (ssFun2 x y) |> Value |> Ok |> Matched
            | _ -> NoMatch
        | _ -> NoMatch)       
    (intFloat2 ops iiFun ifFun fiFun ffFun) <.|.> str2

let operatorErr = 
    // TODO: Implement!
    "Operator error."
    |> ImplementationError

let plusRules (ops : BilboResult<Meaning * Meaning>) : BilboResult<Meaning> =
    intFloatStr2 ops (+) (fun x y -> float(x) + y) (fun x y -> x + float(y)) (+) (+)
    |> Match.underlie ("Not implemented yet." |> ImplementationError |> Error)
    // TODO: Add graph-based plus operations

let minusRules ops =
    intFloat2 ops (-) (fun x y -> float(x) - y) (fun x y -> x - float(y)) (-)
    |> Match.underlie ("Not implemented yet." |> ImplementationError |> Error)
    // TODO: Add graph-based minus operations

let timesRules ops =
    intFloat2 ops (*) (fun x y -> float(x) * y) (fun x y -> x * float(y)) (*)
    |> Match.underlie ("Not implemented yet." |> ImplementationError |> Error)

let zeroCheck ops =
    match ops with
    | Error e -> e |> Error |> Matched
    | Ok (Value lhs', Value rhs') ->
        match lhs',rhs' with
        | (_, Int y) when y=0 ->
            "Cannot divide or modulo by zero" |> ValueError |> Error |> Matched
        | (_, Float y) when y=0.0 ->
            "Cannot divide or modulo by zero" |> ValueError |> Error |> Matched
        | _ -> NoMatch
    | _ -> NoMatch  

let divideRules ops =
    let z = zeroCheck ops
    let eval = lazy(intFloat2 ops (/) (fun x y -> float(x) / y) (fun x y -> x / float(y)) (/))
    z <.|.> eval
    |> Match.underlie ("Not implemented yet." |> ImplementationError |> Error)
    
let moduloRules ops =
    let z = zeroCheck ops
    let eval = lazy(intFloat2 ops (%) (fun x y -> float(x) % y) (fun x y -> x % float(y)) (%))
    z <.|.> eval
    |> Match.underlie ("Not implemented yet." |> ImplementationError |> Error)

let powRules ops =
    let iiFun = fun x y -> float(x) ** float(y) |> Float
    let ifFun = fun x y -> float(x) ** y |> Float
    let fiFun = fun x y -> x ** float(y) |> Float    
    let ffFun = fun x y -> x ** y |> Float    
    intFloat ops iiFun ifFun fiFun ffFun
    |> Match.underlie ("Not implemented yet." |> ImplementationError |> Error)

let ltRules ops =
    let lt = fun x y -> (x < y) |> Bool
    let ifFun = fun x y -> (float(x) < y) |> Bool
    let fiFun = fun x y -> x < float(y) |> Bool
    intFloatStr ops lt ifFun fiFun lt lt
    |> Match.underlie ("Not implemented yet." |> ImplementationError |> Error)

let lteqRules ops =
    let lteq = fun x y -> (x <= y) |> Bool
    let ifFun = fun x y -> (float(x) <= y) |> Bool
    let fiFun = fun x y -> x <= float(y) |> Bool
    intFloatStr ops lteq ifFun fiFun lteq lteq
    |> Match.underlie ("Not implemented yet." |> ImplementationError |> Error)

let gtRules ops =
    // Unpacking the result of lteqRules and inverting it would take as many line...
    let gt = fun x y -> (x > y) |> Bool
    let ifFun = fun x y -> (float(x) > y) |> Bool
    let fiFun = fun x y -> x > float(y) |> Bool
    intFloatStr ops gt ifFun fiFun gt gt
    |> Match.underlie ("Not implemented yet." |> ImplementationError |> Error)

let gteqRules ops =
    let gteq = fun x y -> (x >= y) |> Bool
    let ifFun = fun x y -> (float(x) >= y) |> Bool
    let fiFun = fun x y -> x >= float(y) |> Bool
    intFloatStr ops gteq ifFun fiFun gteq gteq
    |> Match.underlie ("Not implemented yet." |> ImplementationError |> Error)

let equalsRules ops =
    // TODO: Add equality for non-primative (structural types) types
    let eq = fun x y -> (x = y) |> Bool
    let ifFun = fun x y -> (float(x) = y) |> Bool
    let fiFun = fun x y -> x = float(y) |> Bool
    intFloatStr ops eq ifFun fiFun eq eq
    |> Match.underlie ("Not implemented yet." |> ImplementationError |> Error)

let notEqualsRules ops =
    // TODO: Add non-equality for non-primative (structural types) types
    let neq = fun x y -> (x <> y) |> Bool
    let ifFun = fun x y -> (float(x) <> y) |> Bool
    let fiFun = fun x y -> x <> float(y) |> Bool
    intFloatStr ops neq ifFun fiFun neq neq
    |> Match.underlie ("Not implemented yet." |> ImplementationError |> Error)

let boolean ops binOp =
    let booleanTrue (v : Value) =
        // TODO: Add graph pattern matching `and` and `or` operartions for pos and neg graph
        // and non-primative types 
        match v with
        | Int x -> x<>0 |> Matched
        | Float x -> x<>0.0 |> Matched
        | String x -> x<>"" |> Matched
        | Bool x -> x |> Matched
        | _ -> NoMatch
    match ops with
    | Error e -> e |> Error |> Matched
    | Ok (Value lhs, Value rhs) ->
        let lhs' = booleanTrue lhs
        let rhs' = booleanTrue rhs
        match lhs',rhs' with
        | Matched l', Matched r' -> binOp l' r' |> Matched
        | _ -> NoMatch
    | _ -> NoMatch

let andRules (ops : BilboResult<Meaning*Meaning>) =
    boolean ops (fun x y -> (x && y) |> Bool |> Value |> Ok)
    |> Match.underlie ("Not implemented yet." |> ImplementationError |> Error)

let orRules (ops : BilboResult<Meaning*Meaning>) =
    boolean ops (fun x y -> (x || y) |> Bool |> Value |> Ok)
    |> Match.underlie ("Not implemented yet." |> ImplementationError |> Error)

let xorRules (ops : BilboResult<Meaning*Meaning>) =
    let xor x y = (x && (not y)) || ((not x) && y)
    boolean ops (fun x y -> (xor x y) |> Bool |> Value |> Ok)
    |> Match.underlie ("Not implemented yet." |> ImplementationError |> Error)

let nodeConsRules (ops : BilboResult<Meaning*Meaning>) =
    let consNode id load = {id=id; load=load}
    let getNodePart nodeFn obj partStr =
        match obj with
        | Value v ->
            match v with
            | String _ | Float _ | Int _ | Bool _ -> obj |> Ok
            | Value.Node n -> n |> nodeFn |> Ok
            | Transform _ -> nodeConsError "transform" partStr
            | Type _ -> nodeConsError "type definition" partStr
        | Space (Object oTyp, symTab) -> obj |> Ok
        | Space _ -> nodeConsError "namespace" partStr
    let getId node =
        getNodePart (fun (n : Value.Node) -> n.id) node "node id"
    let getLoad node =
        getNodePart (fun (n : Value.Node) -> n.load) node "node load"
    match ops with
    | Error e -> e |> Error
    | Ok(l,r) ->
        match getId l, getLoad r with
        | Ok id, Ok load ->
            {id=id; load=load}
            |> Value.Node
            |> Value
            |> Ok
        | Error e, _ -> e |> Error
        | Ok _, Error e -> e |> Error

let isRules (l : Meaning) (rhs : Expr) =
    match rhs with
    | Var (typ) ->
        match l with
        | Value v -> 
            match v with
            // TODO: is implementation for graphs, type defs, transform defs
            | String _ -> typ = "str" |> Bool |> Value |> Ok
            | Float _ -> typ = "float" |> Bool |> Value |> Ok
            | Int _ -> typ = "int" |> Bool |> Value |> Ok
            | Bool _ -> typ = "bool" |> Bool |> Value |> Ok
        | Space (Object(oTyp), _) ->
            typ = oTyp |> Bool |> Value |> Ok
        | _ ->
            "Can only check type for primative types"
            |> TypeError
            |> Error
    | _ -> 
        "`is` should be followed by a type name"
        |> OperatorError
        |> Error
            



             


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