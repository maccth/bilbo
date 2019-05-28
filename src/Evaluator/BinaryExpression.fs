module Bilbo.Evaluator.BinaryExpression

open Bilbo.Common.Extensions
open Bilbo.Common.Type
open Bilbo.Common.Value
open Bilbo.Common.Error
open Bilbo.Evaluator.Print
open Bilbo.Graph.Graph

let intFloat ops iiFun ifFun fiFun ffFun =
    let lMean,rMean = ops
    match lMean, rMean with
    | Value lhs, Value rhs ->
        match lhs,rhs with
        | Int x, Int y -> (iiFun x y) |> Value |> Ok |> Matched
        | Int x, Float y -> (ifFun x y) |> Value |> Ok |> Matched
        | Float x, Int y -> (fiFun x y) |> Value |> Ok |> Matched
        | Float x, Float y -> (ffFun x y) |> Value |> Ok |> Matched
        | _ -> NoMatch
    | _ -> NoMatch

let intFloatStr ops iiFun ifFun fiFun ffFun ssFun =
    let str = lazy(
        match ops with
        | Value (String x), Value (String y) -> (ssFun x y) |> Value |> Ok |> Matched
        | _ -> NoMatch)
    (intFloat ops iiFun ifFun fiFun ffFun) 
    |??> str
        
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
        | Value lhs', Value rhs' ->
            match lhs',rhs' with
            | String x, String y -> (ssFun2 x y) |> Value |> Ok |> Matched
            | _ -> NoMatch
        | _ -> NoMatch)       
    (intFloat2 ops iiFun ifFun fiFun ffFun)
    |??> str2

let graphMatcher (ops : Meaning * Meaning) rule conv =
    match ops with
    | Value(Graph lhs), Value(Graph rhs) -> rule lhs rhs  |> conv |> Matched
    | _ -> NoMatch

let graphCollectionMatcher (ops : Meaning * Meaning) gRule =
    match ops with
    // Both implementations are here because the order matters.
    // If there are node id clashes we overide the load with that of the latest ('right most') added node.
    | Value(Graph g), Value(Collection c) ->
        c
        |> Set.map (gRule g)
        |> Collection.toMeaning
        |> Ok
        |> Matched
    | Value(Collection c), Value (Graph g) ->
        c
        |> Set.map (fun gIn -> gRule gIn g)
        |> Collection.toMeaning
        |> Ok
        |> Matched
    | _ -> NoMatch         

let collectionMatcher (ops : Meaning * Meaning) gRule =
    match ops with
    | Value(Collection c1), Value(Collection c2) ->
        let c1' = Set.toList c1
        let c2' = Set.toList c2
        List.allPairs c1' c2'
        |> List.map (fun (g1,g2) -> gRule g1 g2)
        |> Collection.ofList
        |> Collection.toMeaning
        |> Ok
        |> Matched
    | _ -> NoMatch     

let nodeMatcher (ops : Meaning * Meaning) rule conv =
    match ops with
    | Value(Node lhs), Value(Node rhs) -> rule lhs rhs  |> conv |> Matched
    | _ -> NoMatch

let opTypes ops = (ops |> fst |> typeStr), (ops |> snd |> typeStr)

let binOpMeanError opSymbol ops =
    let typL, typR = ops |> opTypes
    binOpTypeError opSymbol typL typR

let plusRules (ops : Meaning * Meaning) : BilboResult<Meaning> =
    let ifs = intFloatStr2 ops (+) (fun x y -> float(x) + y) (fun x y -> x + float(y)) (+) (+)
    let g = lazy(graphMatcher ops Graph.addGraphs (Graph >> Value >> Ok))
    let gc = lazy(graphCollectionMatcher ops Graph.addGraphs)
    let c = lazy(collectionMatcher ops Graph.addGraphs)
    ifs
    |??> g
    |??> gc
    |??> c
    |..> binOpMeanError "+" ops
    
let minusRules ops =
    let ifl = intFloat2 ops (-) (fun x y -> float(x) - y) (fun x y -> x - float(y)) (-)
    let g = lazy(graphMatcher ops Graph.subtractGraphs (Graph >> Value >> Ok))
    let gc = lazy(graphCollectionMatcher ops Graph.subtractGraphs)
    let c = lazy(collectionMatcher ops Graph.subtractGraphs)
    ifl
    |??> g
    |??> gc
    |??> c
    |..> binOpMeanError "-" ops

let timesRules ops : BilboResult<Meaning> =
    intFloat2 ops (*) (fun x y -> float(x) * y) (fun x y -> x * float(y)) (*)
    |..> binOpMeanError "*" ops

let zeroCheck ops : Match<BilboResult<'T>> =
    match ops with
    | Value lhs, Value rhs ->
        match lhs, rhs with
        | (_, Int y) when y=0 -> divideModByZero() |> Matched
        | (_, Float y) when y=0.0 -> divideModByZero() |> Matched
        | _ -> NoMatch
    | _ -> NoMatch  

let divideRules ops : BilboResult<Meaning> =
    let z = zeroCheck ops
    let eval = lazy(intFloat2 ops (/) (fun x y -> float(x) / y) (fun x y -> x / float(y)) (/))
    z
    |??> eval
    |..> binOpMeanError "/" ops
    
let moduloRules ops =
    let z = zeroCheck ops
    let eval = lazy(intFloat2 ops (%) (fun x y -> float(x) % y) (fun x y -> x % float(y)) (%))
    z
    |??> eval
    |..> binOpMeanError "%" ops

let powRules ops =
    let iiFun = fun x y -> float(x) ** float(y) |> Float
    let ifFun = fun x y -> float(x) ** y |> Float
    let fiFun = fun x y -> x ** float(y) |> Float    
    let ffFun = fun x y -> x ** y |> Float    
    intFloat ops iiFun ifFun fiFun ffFun
    |..> binOpMeanError "^" ops

let ltRules ops =
    let lt = fun x y -> (x < y) |> Bool
    let ifFun = fun x y -> (float(x) < y) |> Bool
    let fiFun = fun x y -> x < float(y) |> Bool
    intFloatStr ops lt ifFun fiFun lt lt
    |..> binOpMeanError "<" ops

let lteqRules ops =
    let lteq = fun x y -> (x <= y) |> Bool
    let ifFun = fun x y -> (float(x) <= y) |> Bool
    let fiFun = fun x y -> x <= float(y) |> Bool
    intFloatStr ops lteq ifFun fiFun lteq lteq
    |..> binOpMeanError "<=" ops

let gtRules ops =
    // Unpacking the result of lteqRules and inverting it would take as many lines...
    let gt = fun x y -> (x > y) |> Bool
    let ifFun = fun x y -> (float(x) > y) |> Bool
    let fiFun = fun x y -> x > float(y) |> Bool
    intFloatStr ops gt ifFun fiFun gt gt
    |..> binOpMeanError ">" ops

let gteqRules ops =
    let gteq = fun x y -> (x >= y) |> Bool
    let ifFun = fun x y -> (float(x) >= y) |> Bool
    let fiFun = fun x y -> x >= float(y) |> Bool
    intFloatStr ops gteq ifFun fiFun gteq gteq
    |..> binOpMeanError ">=" ops

let equalsRules ops =
    // TODO: Add equality for non-primative (structural types) types
    let eq = fun x y -> (x = y) |> Bool
    let ifFun = fun x y -> (float(x) = y) |> Bool
    let fiFun = fun x y -> x = float(y) |> Bool
    let ifs = intFloatStr ops eq ifFun fiFun eq eq
    let n = lazy(nodeMatcher ops (=) (Bool >> Value >> Ok))
    let g = lazy(graphMatcher ops Graph.equal (Bool >> Value >> Ok))
    let nope = false |> Bool |> Value |> Ok |> Matched
    ifs
    |??> n
    |??> g
    |?> nope
    |..> binOpMeanError "== (or !=)" ops

let notEqualsRules ops =
    equalsRules ops
    |> function
    | Ok (Value(Bool(b))) -> b |> not |> Bool |> Value |> Ok
    | Ok _ -> binOpMeanError "!=" ops
    | Error e -> e |> Error

let boolean ops binOp =
    let booleanTrue (v : Value) =
        match v with
        | Int x -> x<>0 |> Matched
        | Float x -> x<>0.0 |> Matched
        | String x -> x<>"" |> Matched
        | Bool x -> x |> Matched
        | _ -> NoMatch
    match ops with
    | (Value lhs, Value rhs) ->
        let lhs' = booleanTrue lhs
        let rhs' = booleanTrue rhs
        match lhs',rhs' with
        | Matched l', Matched r' -> binOp l' r' |> Matched
        | _ -> NoMatch
    | _ -> NoMatch

let andRules (ops : Meaning * Meaning) =
    boolean ops (fun x y -> (x && y) |> Bool |> Value |> Ok)
    |..> binOpMeanError "and" ops

let orRules (ops : Meaning * Meaning) =
    boolean ops (fun x y -> (x || y) |> Bool |> Value |> Ok)
    |..> binOpMeanError "or" ops

let xorRules (ops : Meaning * Meaning) =
    let xor x y = (x && (not y)) || ((not x) && y)
    boolean ops (fun x y -> (xor x y) |> Bool |> Value |> Ok)
    |..> binOpMeanError "xor" ops

let nodeConsRules (ops : Meaning * Meaning) =
    let getNodePart nodeFn obj partStr =
        match obj with
        | Value v ->
            match v with
            | String _ | Float _ | Int _ | Bool _ -> obj |> Ok
            | Value.Node n -> n |> nodeFn |> Ok
            | Type _ -> nodeConsError "type definition" partStr
            | Pipeline _ -> nodeConsError "function or transform" partStr
            | Graph _ -> nodeConsError "graph" partStr
            | Collection _ -> nodeConsError "collection" partStr
        | Space (Object oTyp, symTab) -> obj |> Ok
        | Space _ -> nodeConsError "namespace" partStr
        | ParamList _ -> nodeConsError "parameter list" partStr
    let getId node =
        getNodePart (fun (n : Node) -> n.id) node "node id"
    let getLoad node =
        getNodePart (fun (n : Node) -> n.load) node "node load"
    match ops with
    | l,r ->
        match getId l, getLoad r with
        | Ok id, Ok load ->
            {id=id; load=load}
            |> Value.Node
            |> Value
            |> Ok
        | Error e, _ -> e |> Error
        | Ok _, Error e -> e |> Error

let collectRules ops =
    let typeError = ops |> (fun (l,r) -> (l |> typeStr, r |> typeStr) ||> nonGraphCollectionError)
    let cOk = Value.Collection >> Value >> Ok
    match ops with
    | Value lVal, Value rVal ->
        match lVal, rVal with
        // TODO: Faster implementation of these by making the set use custom comparison
        | Collection l, Collection r -> l + r |> Set.toList |> Collection.ofList |> cOk
        | Graph l, Collection r -> Set.add l r |> Set.toList |> Collection.ofList |> cOk
        | Collection l, Graph r -> Set.add r l |> Set.toList |> Collection.ofList |> cOk
        | Graph l, Graph r -> [l;r] |> Collection.ofList |> cOk
        | _ -> typeError
    | _ -> typeError

let thenPipeRules ops : BilboResult<Meaning> =
    let lMean, rMean = ops
    match lMean, rMean with
    | Value (Pipeline pl), Value (Pipeline pr) -> (pl,pr) |> ThenPipe |> Pipeline |> Value |> Ok
    // TODO: Decide on this. Working implementation of param list pipeline stages. 
    // TODO: Uncoment corresponding tests
    // | Value (Pipeline pl), ParamList(pLst) -> pl @ [ParamStage pLst] |> Pipeline |> Value |> Ok
    // | Value (Pipeline pl), _ ->
    //     let paramStage = [rMean] |> ParamStage |> fprint (g,20,30) >> t3un p -> [p]
    //     pl @ paramStage |> Pipeline |> Value |> Ok
    // |  ParamList(pLst), Value (Pipeline pr) -> [ParamStage pLst] @ pr |> Pipeline |> Value |> Ok
    // | _, Value (Pipeline pr) ->
    //     let paramStage = [lMean] |> ParamStage |> fun p -> [p]
    //     paramStage @ pr |> Pipeline |> Value |> Ok
    | _ -> ops |> opTypes ||> nonFuncTranInThenPipe

let orPipeRules ops =
    let lMean, rMean = ops
    match lMean, rMean with
    | Value (Pipeline pl), Value (Pipeline pr) -> (pl,pr,[]) |> OrPipe |> Pipeline |> Value |> Ok
    | _ -> ops |> opTypes ||> nonTranInPipe "<|>"


let andPipeRules ops =
    let lMean, rMean = ops
    match lMean, rMean with
    | Value (Pipeline pl), Value (Pipeline pr) -> (pl,pr) |> AndPipe |> Pipeline |> Value |> Ok
    | _ -> ops |> opTypes ||> nonTranInPipe "<&>"

let mulAppRules ops =
    match ops with
    | Value (Pipeline pl), Value (Int m) ->
        match m with
        | x when x <= 0 -> m |> Int |> valuePrint |-> nonPositiveAppMultiplier  
        | x ->
            [1..x] 
            |> List.map (fun _ -> pl)
            |> List.reduce (fun pline p -> (pline,p) |> ThenPipe)
            |> Pipeline
            |> Value
            |> Ok
    | Value (Pipeline pl), r -> r |> typeStr |> nonIntMultipleAppRhs
    |l, _ -> l |> typeStr |> nonPipelineMultipleAppLhs