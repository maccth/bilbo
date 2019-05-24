module Bilbo.Evaluator.BinaryExpression

open Bilbo.Common.Extensions
open Bilbo.Common.Type
open Bilbo.Common.Value
open Bilbo.Common.Error
open Bilbo.Graph.Graph
open Bilbo.Evaluator.Print

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

let nodeMatcher (ops : Meaning * Meaning) rule conv =
    match ops with
    | Value(Node lhs), Value(Node rhs) -> rule lhs rhs  |> conv |> Matched
    | _ -> NoMatch

let plusRules (ops : Meaning * Meaning) : BilboResult<Meaning> =
    let ifs = intFloatStr2 ops (+) (fun x y -> float(x) + y) (fun x y -> x + float(y)) (+) (+)
    let g = lazy(graphMatcher ops Graph.addGraphs (Result.bind (Graph >> Value >> Ok)))
    ifs
    |??> g
    |..> ("Cannot use operator + between types " + (ops |> fst |> typeStr) + " and " +  (ops |> snd |> typeStr) + "." |> TypeError |> Error)

let minusRules ops =
    let ifl = intFloat2 ops (-) (fun x y -> float(x) - y) (fun x y -> x - float(y)) (-)
    let g = lazy(graphMatcher ops Graph.subtractGraphs (Result.bind (Graph >> Value >> Ok)))
    ifl
    |??> g
    |..> ("Minus rules" |> notImplementedYet)

let timesRules ops =
    intFloat2 ops (*) (fun x y -> float(x) * y) (fun x y -> x * float(y)) (*)
    |..> ("Times rules" |> notImplementedYet)

let zeroCheck ops =
    match ops with
    | Value lhs, Value rhs ->
        match lhs, rhs with
        | (_, Int y) when y=0 ->
            "Cannot divide or modulo by zero" |> ValueError |> Error |> Matched
        | (_, Float y) when y=0.0 ->
            "Cannot divide or modulo by zero" |> ValueError |> Error |> Matched
        | _ -> NoMatch
    | _ -> NoMatch  

let divideRules ops =
    let z = zeroCheck ops
    let eval = lazy(intFloat2 ops (/) (fun x y -> float(x) / y) (fun x y -> x / float(y)) (/))
    z
    |??> eval
    |> Match.underlie ("Divide rules" |> notImplementedYet)
    
let moduloRules ops =
    let z = zeroCheck ops
    let eval = lazy(intFloat2 ops (%) (fun x y -> float(x) % y) (fun x y -> x % float(y)) (%))
    z
    |??> eval
    |> Match.underlie ("Modulo rules" |> notImplementedYet)

let powRules ops =
    let iiFun = fun x y -> float(x) ** float(y) |> Float
    let ifFun = fun x y -> float(x) ** y |> Float
    let fiFun = fun x y -> x ** float(y) |> Float    
    let ffFun = fun x y -> x ** y |> Float    
    intFloat ops iiFun ifFun fiFun ffFun
    |> Match.underlie ("Pow rules" |> notImplementedYet)

let ltRules ops =
    let lt = fun x y -> (x < y) |> Bool
    let ifFun = fun x y -> (float(x) < y) |> Bool
    let fiFun = fun x y -> x < float(y) |> Bool
    intFloatStr ops lt ifFun fiFun lt lt
    |> Match.underlie ("Less than rules" |> notImplementedYet)

let lteqRules ops =
    let lteq = fun x y -> (x <= y) |> Bool
    let ifFun = fun x y -> (float(x) <= y) |> Bool
    let fiFun = fun x y -> x <= float(y) |> Bool
    intFloatStr ops lteq ifFun fiFun lteq lteq
    |> Match.underlie ("Less than or equal rules" |> notImplementedYet)

let gtRules ops =
    // Unpacking the result of lteqRules and inverting it would take as many lines...
    let gt = fun x y -> (x > y) |> Bool
    let ifFun = fun x y -> (float(x) > y) |> Bool
    let fiFun = fun x y -> x > float(y) |> Bool
    intFloatStr ops gt ifFun fiFun gt gt
    |> Match.underlie ("Greater than rules" |> notImplementedYet)

let gteqRules ops =
    let gteq = fun x y -> (x >= y) |> Bool
    let ifFun = fun x y -> (float(x) >= y) |> Bool
    let fiFun = fun x y -> x >= float(y) |> Bool
    intFloatStr ops gteq ifFun fiFun gteq gteq
    |> Match.underlie ("Greater than or equal rules" |> notImplementedYet)

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
    |..> ("Equal rules" |> notImplementedYet)

let notEqualsRules ops =
    equalsRules ops
    |> function
    | Ok (Value(Bool(b))) -> b |> not |> Bool |> Value |> Ok
    | Ok (_) -> "Not equal rules" |> notImplementedYet
    | Error (ImplementationError _) ->  "Not equal rules" |> notImplementedYet
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
    |> Match.underlie ("And rules" |> notImplementedYet)

let orRules (ops : Meaning * Meaning) =
    boolean ops (fun x y -> (x || y) |> Bool |> Value |> Ok)
    |> Match.underlie ("Or rules" |> notImplementedYet)

let xorRules (ops : Meaning * Meaning) =
    let xor x y = (x && (not y)) || ((not x) && y)
    boolean ops (fun x y -> (xor x y) |> Bool |> Value |> Ok)
    |> Match.underlie ("Xor rules" |> notImplementedYet)

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

let pipeRules ops =
    let lMean, rMean = ops
    match lMean, rMean with
    | Value (Pipeline pl), Value (Pipeline pr) -> (pl,pr) |> ThenPipe |> Pipeline |> Value |> Ok
    // TODO: Decide on this. Working implementation of param list pipeline stages. 
    // TODO: Uncoment corresponding tests
    // | Value (Pipeline pl), ParamList(pLst) -> pl @ [ParamStage pLst] |> Pipeline |> Value |> Ok
    // | Value (Pipeline pl), _ ->
    //     let paramStage = [rMean] |> ParamStage |> fun p -> [p]
    //     pl @ paramStage |> Pipeline |> Value |> Ok
    // |  ParamList(pLst), Value (Pipeline pr) -> [ParamStage pLst] @ pr |> Pipeline |> Value |> Ok
    // | _, Value (Pipeline pr) ->
    //     let paramStage = [lMean] |> ParamStage |> fun p -> [p]
    //     paramStage @ pr |> Pipeline |> Value |> Ok
    | _ ->
        "Only functions or transforms can be composed in a pipeline"
        |> TypeError
        |> Error

let rec mulAppRules ops =
    match ops with
    | Value (Pipeline pl), Value (Int m) ->
        match m with
        | x when x <= 0 -> m |> Int |> valuePrint |-> nonPositiveMultipleApp  
        | x ->
            [1..x] 
            |> List.map (fun _ -> pl)
            |> List.reduce (fun pline p -> (pline,p) |> ThenPipe)
            |> Pipeline
            |> Value
            |> Ok
    | Value (Pipeline pl), r -> r |> typeStr |> nonIntMultipleAppRhs
    |l, _ -> l |> typeStr |> nonPipelineMultipleAppLhs