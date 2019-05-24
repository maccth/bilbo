module Bilbo.Evaluator.ExpressionStatement

open Bilbo.Common.Extensions
open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.Type
open Bilbo.Common.SymbolTable
open Bilbo.Common.Error
open Bilbo.Evaluator.PrimativeTypes
open Bilbo.Evaluator.BinaryExpression
open Bilbo.Evaluator.PrefixExpression
open Bilbo.Evaluator.Print
open Bilbo.Graph.Graph
open Bilbo.Graph.Isomorphism

let rec evalPStageBody (syms : Symbols) spLst st bod =
    let syms' : Symbols = st :: syms
    let folder syms (es : ExprStatementL) =
        match syms with
        | Error e -> e |> Error
        | Ok syms' ->
            let loc,es' = es
            evalExprStatement syms' spLst es'
    List.fold folder (Ok syms') bod

and evalFuncBody (syms : Symbols) spLst fst bod ret =
    let symsPostFunc = evalPStageBody syms spLst fst bod
    match symsPostFunc with
    | Error e -> e |> Error
    | Ok syms'' -> evalExpr syms'' spLst ret

and inspectTranOutput (out : Meaning list) : BilboResult<Meaning> =
    match out with
    | [] -> "There has been a terrible match error. Nothing matched." |> MatchError |> Error
    | [one] -> one |> Ok
    | _ ->
        let graphCheck lst mIn =
            match lst with
            | Error e -> e |> Error
            | Ok lst' ->
                match mIn with
                | Value(Graph(g)) -> g :: lst' |> Ok
                | _ ->
                    "You cannot possible have something of type "
                    + (typeStr mIn) + " in a collection. Fool."
                    |> TypeError |> Error
        List.fold graphCheck (Ok []) out
        |=> Collection.ofList
        |=> Collection.toMeaning
  
and evalMatchStatement (syms : Symbols) spLst (mat : MatchStatement) : BilboResult<Meaning list> =
    let matchGExpr,cases = mat
    let syms' = SymbolTable.empty :: syms
    let matchG = evalExpr syms' spLst matchGExpr
    let matchOut =
        match matchG with
        | Error e -> e |> Error
        | Ok (Value(Graph g)) -> evalMatchCases syms' spLst cases g
        | Ok (Value(Collection c)) ->
            let folder mLstIn g =
                match mLstIn with
                | Error e -> e |> Error
                | Ok mLstOut ->
                    let mLstRes = evalMatchCases syms' spLst cases g
                    match mLstRes with
                    | Error e -> e |> Error
                    | Ok mLst -> mLst @ mLstOut |> Ok
            List.fold folder (Ok []) (Set.toList c)
        | Ok mean -> mean |> typeStr |> notMatchingWithinGraph
    matchOut 
    // matchOut
    // |-> inspectMeaningLst

and evalMatchCases syms spLst (cases : MatchCase list) (hg : Graph) : BilboResult<Meaning list> =
    let folder mLst case = 
        match mLst with
        | Error e -> e |> Error
        | Ok [] ->
            match evalMatchCase syms spLst hg case with
            | Error e -> e |> Error
            | Ok newMLst -> newMLst |> Ok
        | Ok mLst' -> mLst' |> Ok
    List.fold folder (Ok []) cases        

and addNodesToSyms spLst (nLst : Node list) (nMap : NodeMap) (syms : Symbols) =
    let nLst' : (Node*UnboundNodeId) list = nLst |> List.map (fun n -> n, Map.find n.id nMap)
    let folder syms (n,ubnid) =
        match syms with
        | Error e -> e |> Error
        | Ok syms' ->
            match strFromMean ubnid with
            | None -> "Unbound node id is not a string variable" |> implementationError
            | Some id -> Symbols.set syms' {id=id; spLst=spLst} (n |> Node |> Value)
    List.fold folder (Ok syms) nLst'

and addEdgesToSyms spLst (pg : UnboundGraph) (eLst : Edge list) (eMap : Map<Edge,UnboundEdgeId>) (syms : Symbols) =
    let getEdgeSymbolId e =
        let ubeid = Map.find e eMap
        UnboundGraph.edge ubeid pg
        |> fun ube -> ube.weight
        |> Option.bind strFromMean
    let eLst' : (Edge*Id option) list = eLst |> List.map (fun e -> e, getEdgeSymbolId e)
    let folder syms (e:Edge,id) =
        match syms with
        | Error e -> e |> Error
        | Ok syms' ->
            match id with
            | None -> syms' |> Ok
            | Some id' ->
                match e.weight with
                | None -> "Unbound weighted edge was bound to unweighted edge" |> implementationError
                | Some w ->
                    Symbols.set syms' {id=id'; spLst=spLst} (w)
    List.fold folder (Ok syms) eLst'

and evalMatchCase syms spLst (hg : Graph) (case : MatchCase) : BilboResult<Meaning list> =
    match case with
    | CatchAll (where,bod,term) ->
        match evalWhere syms spLst where with
        | Error e -> e |> Error
        | Ok false -> [] |> Ok
        | Ok true ->
            match term with
            | Become _ -> "Cannot use become in a catch-all match case" |> SyntaxError |> Error
            | Return ret ->
                let stHd, rest = Symbols.top syms           
                evalFuncBody rest spLst stHd bod ret
                |=> fun r -> [r]
    | Pattern (posPgExpr,negPgExpr,where,bod,term) ->
        let posMapRes = evalPatternGraph (*syms spLst*) hg posPgExpr
        let negMapRes =
            match negPgExpr with
            | None -> [] |> Ok
            | Some npg -> evalPatternGraph (*syms spLst*) hg npg
        match posMapRes, negMapRes with
        | Error e, _ -> e |> Error
        | _, Error e -> e |> Error
        | Ok posMaps, Ok negMaps ->
            let negMaps' = List.map (fun (nMap,eMap,pg) -> (nMap,eMap)) negMaps
            match posMaps with
            | [] -> [] |> Ok
            | _ ->
                let folder (cIn : BilboResult<Meaning list>) (nMap,eMap,pg) =
                    match cIn with
                    | Error e -> e |> Error
                    | Ok mLst ->
                        let allNegMapsInconsistent = mapInconsistent nMap negMaps'
                        match (List.isEmpty negMaps || allNegMapsInconsistent) with
                        | false -> mLst |> Ok
                        | true ->
                            let m = evalMatch syms spLst hg (pg) where bod term nMap eMap
                            match m with
                            | None -> mLst |> Ok
                            | Some (Error e) -> e |> Error
                            | Some (Ok mean) -> mean :: mLst |> Ok
                List.fold folder (Ok []) posMaps

and mapInconsistent (nMap : NodeMap) (negMappings : (NodeMap * EdgeMap) list) =
    // Need to check if the same identifier (in the pattern graph) maps to
    // the same node/weight in the host graph. If this is the case, then
    // the negative mapping is consistent, and the match is not valid
    // Need to check two things, first the forward mappings:
    //  If two nodes are mapped to by the same identifier, then those two nodes are the same
    // and then the negative backward mapping:
    //  If two nodes are not mapped to by the same identifier, then those two nodes are different
    let negNMaps  = List.map fst negMappings
    let outerFolder consistentMap negNMap =
        match consistentMap with
        | true -> true
        | false ->
            let folder consistentMap' (pgNode,hgNode) =
                let innerFolder conMap'' (negPgNode,negHgNode) =
                    let bothSame = negPgNode = pgNode && negHgNode = hgNode
                    let bothDiff = negPgNode <> pgNode && negHgNode <> hgNode
                    conMap'' && (bothSame || bothDiff)
                List.fold innerFolder consistentMap' (negNMap |> Map.toList)    
            List.fold folder true (nMap |> Map.toList)
    List.fold outerFolder false negNMaps 
    |> not

and evalMatch syms spLst hg pg where bod term nMap eMap : BilboResult<Meaning> option =
    let (sgRes, revNMap, revEMap) = UnboundGraph.bind hg pg nMap eMap
    match sgRes with    
    | Error e -> e |> Error |> Some
    | Ok sg ->
        let symsWithSgElems =
            syms
            |> addNodesToSyms spLst (Graph.nodes sg) revNMap
            |-> addEdgesToSyms spLst pg (Graph.edges sg) revEMap
        match symsWithSgElems with
        | Error e -> e |> Error |> Some
        | Ok [] -> "The symbol table list cannot be empty inside a pipeline" |> implementationError |> Some
        | Ok (stHd :: rest) ->
            match evalWhere (stHd :: rest) spLst where with
            | Error e -> e |> Error |> Some
            | Ok false -> None
            | Ok true ->                             
                match term with
                | Become bExpr ->
                    evalPStageBody rest spLst stHd bod
                    |-> fun syms' -> evalBecome syms' spLst hg sg bExpr
                    |> Some
                | Return ret ->
                    evalFuncBody rest spLst stHd bod ret
                    |> Some

and evalBecome syms spLst hg sg bExpr =
    let bgRes = evalExpr syms spLst bExpr
    match bgRes with
    | Error e -> e |> Error
    | Ok (Value(Graph bg)) ->
        let ids nodes =  nodes |> Set.map (fun (n : Node) -> n.id)
        let nR = bg |> Graph.nodes |> Set.ofList
        let nL = sg |> Graph.nodes |> Set.ofList
        let nHg = hg |> Graph.nodes |> Set.ofList

        let nRIds = nR |> ids
        let nLIds = nL |> ids
        let nHgIds = nHg |> ids
        let nPresIds = (nHgIds - nLIds)

        let nIdsOut = nPresIds + nRIds
        let nPres = nHg |> Set.filter (fun n -> Set.contains n.id nPresIds)
        let nOut = nPres + nR

        let eR = Graph.edges bg
        let eL = Graph.edges sg 
        let eHg = Graph.edges hg 

        let changeEdgeNodes (e : Edge) =
            let source' = 
                match Set.contains e.source.id nRIds with
                | true -> Graph.node e.source.id bg
                | false -> e.source
            let target' = 
                match Set.contains e.target.id nRIds with
                | true -> Graph.node e.target.id bg
                | false -> e.target  
            {Edge.source=source'; weight=e.weight; target=target'}                      

        let eOut =
            (eHg /-/ eL)
            |> (@) eR
            |> List.map changeEdgeNodes
            |> List.filter (fun e -> Set.contains e.source nOut && Set.contains e.target nOut)
        Graph.empty
        |> Graph.addEdges eOut
        |-> Graph.addNodes (nOut |> Set.toList)
        |=> Graph
        |=> Value
    | _ -> "special [+] and [-] graphs" |> notImplementedYet    

and evalWhere syms spLst where =
    match where with
    | None -> true |> Ok
    | Some e ->
        match evalExpr syms spLst e with
        // TODO: add 'in where clause' error once error lists have been implemented
        | Error e -> e |> Error
        | Ok (Value v) ->
            match typeConvert CastBool v with
            | Error e -> e |> Error
            | Ok (Bool b) -> b |> Ok
            | _ -> v |> typeStrValue |> nonBoolInWhereClause
        | Ok mean -> mean |> typeStr |> nonBoolInWhereClause

and recomputeMaps hg lMaps rMaps combine =
    // Consistent addition is consistent union is more complicated than recomputing the maps
    // for the combined (added/subtracted) graphs
    let lGraphs = List.map (fun (nMap,eMap,pg) -> pg) lMaps
    let rGraphs = List.map (fun (nMap,eMap,pg) -> pg) rMaps 
    let combinedGs = List.allPairs lGraphs rGraphs |> List.map (fun (lg,rg) -> combine lg rg)
    let maps = List.map (fun pg -> Graph.unboundSgiAll hg pg |> Set.toList, pg) combinedGs 
    let mapsWithPg =
        List.collect (fun (map,pg) -> map |> List.map (fun map -> fst map, snd map, pg)) maps
        |> List.distinct
    mapsWithPg |> Ok

and evalPatternGraph (*syms spLst*) (hg : Graph) pgExpr : BilboResult< (NodeMap * EdgeMap * UnboundGraph) list> =
    match pgExpr with
    | PGraph pe ->
        pe
        |> evalPatternPathExpr
        |=> fun pg ->
            let a = Graph.unboundSgiAll hg pg |> Set.toList
            let b = List.map (fun m -> (fst m, snd m, pg)) a
            b
    | PGraphBinExpr (pgL,op,pgR) ->
        let lMaps' = evalPatternGraph hg pgL
        let rMaps' = evalPatternGraph hg pgR
        match lMaps', rMaps' with
        | Error e, _ -> e |> Error
        | _, Error e -> e |> Error
        | Ok lMaps, Ok rMaps ->
            match op with
            | PGAnd
            | PGAdd -> recomputeMaps hg lMaps rMaps UnboundGraph.addGraphs
            | PGSub -> recomputeMaps hg lMaps rMaps UnboundGraph.subtractGraphs
            | PGOr -> lMaps @ rMaps |> Ok

and evalPatternPathExpr (*syms spLst*) (pe : PathExpr) : BilboResult<UnboundGraph> =
    let evalExprWithinPg (e : Expr) =
        match e with
        | Var v -> v |> String |> Value |> Ok
        | _ -> "Can only use identifiers to be bound in pattern graph" |> TypeError |> Error
    let node nid = {nid=nid}
    let edge s w t = {source = node s; weight = w; target = node t}
    let evalPgEdgeOp ln edgeOp rn =
        let w : BilboResult<UnboundEdgeWeight> =
            match edgeOp with
            | Left None | Right None | Bidir None ->
                None |> Ok
            | Left (Some we) | Right (Some we) | Bidir (Some we) ->
                we
                |> evalExprWithinPg
                |=> Some
        match w, edgeOp with
        | Error e, _  -> e |> Error
        | Ok w', Right _ -> [edge ln w' rn] |> Ok            
        | Ok w', Left _ -> [edge rn w' ln] |> Ok
        | Ok w', Bidir _ -> [edge ln w' rn; edge rn w' ln] |> Ok
    match pe with
    | PathComp _ -> "Path comprehensions in pattern graph will not be implemented" |> notImplementedYet
    | Path peLst ->
        let evalPathElem (*syms spLst*) (elem : PathElem) (ug : UnboundGraph) =
            match elem with
            | PathElem.Node(n) ->
                match evalExprWithinPg n with
                | Error e -> e |> Error
                | Ok id ->
                    ug
                    |> UnboundGraph.addNode (node id)
                    |> Ok
            | PathElem.Edge(lNode, edg, rNode) ->
                let lnRes = evalExprWithinPg lNode
                let rnRes = evalExprWithinPg rNode
                match lnRes, rnRes with
                | Error e, _ -> e |> Error
                | _, Error e -> e |> Error
                | Ok ln, Ok rn ->
                    match evalPgEdgeOp ln edg rn with
                    | Error e -> e |> Error
                    | Ok eLst ->
                        ug
                        |> UnboundGraph.addEdges eLst
                        |> Ok
        let folder ug elem =
            Result.bind (fun ug -> evalPathElem (*syms spLst*) elem ug) ug     
        List.fold folder (Ok UnboundGraph.empty) peLst

and applyArgToPipeline syms spLst (pLine : Pipeline) (param : Meaning) : BilboResult<PipelineOutput<Meaning>> =
    match pLine with
    | OrPipe _ ->  "Or pipelines" |> notImplementedYet
    | ThenPipe (plFirst,plThen) ->
        let applied = applyArgToPipeline syms spLst plFirst param
        match applied with
        | Error e -> e |> Error
        | Ok (Unfinished(Value(Pipeline(p)))) -> (p,plThen) |> ThenPipe |> Pipeline |> Value |> Unfinished |> Ok
        | Ok (Output(nextParam)) -> applyArgToPipeline syms spLst plThen nextParam
        | Ok (Unfinished(_)) -> "Only pipelines can have an unfinished output." |> implementationError
    | Modified (modPLine, modif) ->
        match modif with
        | Maybe -> "The maybe ? modifier" |> notImplementedYet
        | Once -> "The once $ modifier" |> notImplementedYet
        | UpTo _ -> "The up-to *!* modifier" |> notImplementedYet
        | Alap _alapParam ->
            let mutable output = applyArgToPipeline syms spLst modPLine param
            let mutable outputPrev = param |> Output |> Ok
            let keepApplying output =
                match output with
                | Ok (Output _) -> true 
                | Ok (Unfinished _) -> false
                | Error (MatchError _) -> false
                | Error _ -> false
            while keepApplying output do
                outputPrev <- output
                output <- 
                    match outputPrev with
                    | Ok (Output(mean)) -> applyArgToPipeline syms spLst modPLine mean
                    | _ -> "The previous output is of type Error but the ALAP loop was entered" |> implementationError
            match output with
            | Ok (Unfinished unfinPl) -> unfinPl |> Unfinished |> Ok
            | Error (MatchError _ ) -> outputPrev
            | Error _ -> output
            | Ok (Output _) -> "The output is of type Ok but the ALAP loop was exited" |> implementationError        
    | PStage p ->
        match p with
        | Transform(tDef, tst) ->
            let tName,tParams,preMatchBod,tMatch = tDef
            match tParams with
            | [] -> zeroParamTransformError()
            | hd :: paramsLeft ->
                let tst' = SymbolTable.set tst {id=hd; spLst=spLst} param
                match tst' with
                | Error e -> e |> Error
                | Ok tstUpdated ->
                    match paramsLeft with
                    | [] ->
                        let symsUpdated = evalPStageBody syms spLst tstUpdated preMatchBod
                        match symsUpdated with
                        | Error e -> e |> Error
                        | Ok syms' ->
                            evalMatchStatement syms' spLst tMatch
                            |-> inspectTranOutput
                            |=> Output
                    | _ ->
                        ((tName, paramsLeft,preMatchBod,tMatch), tstUpdated)
                        |> Transform
                        |> PStage
                        |> Pipeline
                        |> Value
                        |> Unfinished
                        |> Ok 
        | Function(fDef, fst) ->
            let fId, fParams, bod, ret = fDef
            match fParams with
            | [] -> zeroParamFunctionError()
            | hd :: paramsLeft ->
                let fst' = SymbolTable.set fst {id=hd; spLst=spLst} param
                match fst' with
                | Error e -> e |> Error
                | Ok fstUpdated ->
                    match paramsLeft with
                    | [] ->
                        let funcResultAsParam = evalFuncBody syms spLst fstUpdated bod ret
                        funcResultAsParam
                        |=> Output
                    | _ ->
                        ((fId, paramsLeft, bod, ret), fstUpdated)
                        |> Function
                        |> PStage
                        |> Pipeline
                        |> Value
                        |> Unfinished
                        |> Ok

and unpackPLineOutput m : BilboResult<Meaning> =
    match m with
    | Error e -> e |> Error
    | Ok (Unfinished m) -> m |> Ok
    | Ok (Output m) -> m |> Ok

and applyArgsToPipeline syms spLst pLine args : BilboResult<Meaning> =
    match pLine with
    | Modified (pl', modif) ->
        match modif with
        | Maybe -> "The maybe ? modifier" |> notImplementedYet
        | Once -> "The once $ modifier" |> notImplementedYet
        | UpTo _ -> "The up-to *!* modifier" |> notImplementedYet
        | Alap _alapParam ->
            let mutable output = applyArgsToPipeline syms spLst pl' args
            let mutable outputPrev = List.head args |> Ok
            let keepApplying output =
                match output with
                | Error _ -> false
                | Ok _ -> true
            while keepApplying output do
                outputPrev <- output
                output <-
                match outputPrev with
                | Ok input ->
                    let args' =
                        match args with
                        | [] -> [input]
                        | first :: rest ->(input::rest)
                    applyArgsToPipeline syms spLst pl' args'
                | _ -> "The previous output is of type Error but the ALAP loop was entered" |> implementationError                
            match output with
            | Error (MatchError _) -> outputPrev
            | Error _ -> output
            | _ -> outputPrev                     
    | _ ->
        match args with
        | [] -> "Control should have passed onto single argument handler." |> implementationError
        | [arg] ->
            applyArgToPipeline syms spLst pLine arg
            |> unpackPLineOutput
        | arg :: rest ->
            let evalRes = applyArgToPipeline syms spLst pLine arg
            match evalRes with
            | Error e -> e |> Error
            | Ok (Unfinished(Value(Pipeline pLine'))) -> applyArgsToPipeline syms spLst pLine' rest
            | Ok (Output(m)) -> m |> Ok
            | _ ->
                "Too many arguments enpiped into function or transform."
                |> TypeError
                |> Error

and enpipeRules syms spLst (l : Expr) (r : Expr) =
    let lRes = evalExpr syms spLst l
    let rRes = evalExpr syms spLst r
    match lRes, rRes with
    | Error e, _ -> e |> Error
    | _, Error e -> e |> Error
    | Ok lMean, Ok rMean ->
        match lMean, rMean with
        | ParamList (pLst), Value (Pipeline pLine) -> applyArgsToPipeline syms spLst pLine pLst
        | _, Value (Pipeline pLine) ->
            applyArgToPipeline syms spLst pLine lMean
            |> unpackPLineOutput
        | _ -> rMean |> typeStr |> nonPStageOnEnpipeRhs

and varAsString var =
    match var with
    | Var(str) -> str |> Some 
    | _ -> None

and strFromMean (mean : Meaning) =
    match mean with
    | Value(String str) -> str |> Some
    | _ -> None

and isRules syms spLst lhs rhs =
    let lhsTypeStr : BilboResult<string> =
        let lhs' = evalExpr syms spLst lhs
        match lhs' with
        | Error e -> e |> Error
        | Ok lMean ->
            match lMean with
            | Value lVal ->
                match lVal with
                | String _ -> "str" |> Ok
                | Float _ -> "float" |> Ok
                | Int _ -> "int" |> Ok
                | Bool _ -> "bool" |> Ok
                | _ ->
                    "Cannot use `is` operator on nodes, graphs, type definitions, functions, transforms or pipelines."
                    |> TypeError
                    |> Error
            | Space (Object(oTyp), _) ->  oTyp |> Ok
            | _ ->
                "Can only check type for primative types"
                |> TypeError
                |> Error
    let rhsTypeStr = lazy (
        let rhs' = evalExpr syms spLst rhs
        match rhs' with
        | Ok (Value(Type(rTyp,_))) -> rTyp |> Ok
        | Error _->
            match varAsString rhs with
            | Some rTyp -> rTyp |> Ok
            | _ ->
                "`is` should be followed by a type name"
                |> OperatorError
                |> Error 
        | _ ->
            "`is` should be followed by a type name"
            |> OperatorError
            |> Error )
    match lhsTypeStr with
    | Error e -> e |> Error
    | Ok lTyp ->
        match rhsTypeStr.Force() with
        | Error e -> e |> Error
        | Ok rTyp ->
            lTyp = rTyp |> Bool |> Value |> Ok

and hasRules syms spLst lhs rhs =
    match varAsString rhs with
    | None ->
        "`has` should be followed by an attribute name"
        |> OperatorError
        |> Error
    | Some attr ->
        let lhs' = evalExpr syms spLst lhs
        match lhs' with
        | Error e -> e |> Error
        | Ok lMean ->
            match lMean with
            | Space (Object(oTyp), st) -> Map.containsKey attr st |> Bool |> Value |> Ok
            | _ ->
                "`has` must have an object on the left-hand side"
                |> OperatorError
                |> Error    

and evalTypeCast syms spLst typ e =
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

and evalObjExpr syms spLst (oe : ObjExpr)  =
    match oe with
    | ObjInstan (typ,attrs) -> evalObjInstan syms spLst typ attrs
    | ParamObjInstan (typ,attrs) ->
        let typeDef = Symbols.find syms {spLst=spLst; id=typ}
        match typeDef with
        | Error e -> e |> Error
        | Ok (Value(Type(_, expAttrs))) ->
            let givenAttrs = attrs |> Map.ofList
            let folder attrLst attr =
                match attrLst with
                | Error e -> e |> Error
                | Ok lst ->
                    match Map.tryFind attr givenAttrs with
                    | None ->
                        "No value bound to attribute \"" + attr + "\" in object instantiation of type " + typ
                        |> FieldError
                        |> Error
                    | Some v -> (v) :: lst |> Ok 
            let attrLst = List.fold folder (Ok []) expAttrs
            match attrLst with
            | Error e -> e |> Error
            | Ok attrLst' -> evalObjInstan syms spLst typ (List.rev attrLst')
        | _ -> typ |> typeNotDefined                                                                              

and evalObjInstan syms spLst typ attrs : BilboResult<Meaning> =
    let evalAttrs attrsIn : BilboResult<Meaning list> =
        let rec evalAttrsRec attrsIn attrsOut =
            match attrsIn with
            | [] ->
                attrsOut |> Ok
            | a :: rest ->
                let a' = evalExpr syms spLst a
                match a' with
                | Ok at -> evalAttrsRec rest (at :: attrsOut)
                | Error e -> e |> Error
        evalAttrsRec attrsIn []
        |> Result.bind (List.rev >> Ok) 
    let typeDef = Symbols.find syms {spLst=spLst; id=typ}
    match typeDef with
    | Error e ->
        // Make error specific to non-existent type...
        e |> Error    
    | Ok (Value(Type (tName, attrNames))) ->
        let nameCount = List.length attrNames
        let givenCount = List.length attrs
        match nameCount = givenCount with
        | false -> objInstanArgNumError typ nameCount givenCount
        | true ->
            match evalAttrs attrs with
            | Error e -> e |> Error
            | Ok attrs' ->
                attrs'
                |> List.zip attrNames
                |> Map.ofList
                |> fun sp -> (Object tName, sp)
                |> Space
                |> Ok
    | _ -> typ |> typeNotDefined

and evalEdgeOp syms spLst (lhs : Node) (rhs : Node) (eOp : EdgeOp) =
    let edge s w t =
        match w with
        | None -> [{Edge.source=s; target=t; weight=None}] |> Ok
        | Some we ->
            let wRes = evalExpr syms spLst we
            match wRes with
            | Error e -> e |> Error
            | Ok w' -> [{Edge.source=s; target=t; weight=Some w'}] |> Ok
    match eOp with
    | Right w -> edge lhs w rhs     
    | Left w -> edge rhs w lhs
    | Bidir w ->
        let r = edge lhs w rhs
        let l = edge rhs w lhs
        match r,l with
        | Error e, _ -> e |> Error
        | _, Error e -> e |> Error
        | Ok r', Ok l' -> r' @ l' |> Ok

and evalGExpr syms spLst ge : BilboResult<Meaning> =
    match ge with
    | PathExpr pe ->
        match pe with
        | PathComp _ -> "Path comprehensions" |> notImplementedYet
        | Path peLst ->
            let pathScan (g : BilboResult<Graph>) (pe : PathElem) =
                match g with
                | Error e -> e |> Error
                | Ok g' -> 
                    match pe with
                    | PathElem.Edge (lhs,eOp,rhs) ->
                        let lhsRes = evalExpr syms spLst lhs
                        let rhsRes = evalExpr syms spLst rhs
                        match lhsRes,rhsRes with
                        | Error e, _ -> e |> Error
                        | _, Error e -> e |> Error
                        | Ok (Value(Node l)), Ok (Value(Node r)) ->
                            let eLstRes = evalEdgeOp syms spLst l r eOp
                            match eLstRes with
                            | Error e -> e |> Error
                            | Ok eLst -> Graph.addEdges eLst g'
                        | Ok l, Ok r -> (l |> typeStr, r |> typeStr) ||> nonNodeInPathEdge
                    | PathElem.Node e ->
                        let n = evalExpr syms spLst e
                        match n with
                        | Error e -> e |> Error
                        | Ok (Value(Node n')) -> Graph.addNode n' g'
                        | Ok n' -> n' |> typeStr |> nonNodeInPath 
            peLst
            |> List.fold pathScan (Ok Graph.empty)
            |> Result.bind (Graph >> Value >> Ok)

and evalBinOperands syms spLst lhs rhs =
    let valL = evalExpr syms spLst lhs
    match valL with
    | Error e -> e |> Error
    | Ok valL' ->
        let valR = evalExpr syms spLst rhs
        match valR with
        | Ok valR' -> (valL', valR') |> Ok
        | Error e -> e |> Error

and (|..>) (syms,spLst,lhs,rhs) opRule =
    let ops = evalBinOperands syms spLst lhs rhs
    match ops with
    | Ok (l, r) -> opRule (l,r)
    | Error e -> e |> Error

and (|.>) (syms,spLst,expr) opRule =
    let meanRes = evalExpr syms spLst expr
    match meanRes with
    | Error e -> e |> Error
    | Ok mean -> mean |> opRule 

and evalBinExpr syms spLst lhs op rhs =
    match op with
    | Pow -> (syms,spLst,lhs,rhs) |..> powRules
    | Times -> (syms,spLst,lhs,rhs) |..> timesRules
    | Divide -> (syms,spLst,lhs,rhs) |..> divideRules
    | Plus -> (syms,spLst,lhs,rhs) |..> plusRules
    | Minus -> (syms,spLst,lhs,rhs) |..> minusRules
    | Percent -> (syms,spLst,lhs,rhs) |..> moduloRules
    | LessThan -> (syms,spLst,lhs,rhs) |..> ltRules
    | LessThanEq -> (syms,spLst,lhs,rhs) |..> lteqRules
    | GreaterThan -> (syms,spLst,lhs,rhs) |..> gtRules
    | GreaterThanEq -> (syms,spLst,lhs,rhs) |..> gteqRules
    | Equal -> (syms,spLst,lhs,rhs) |..> equalsRules
    | NotEqual -> (syms,spLst,lhs,rhs) |..> notEqualsRules
    | And -> (syms,spLst,lhs,rhs) |..> andRules
    | Or -> (syms,spLst,lhs,rhs) |..> orRules
    | Xor -> (syms,spLst,lhs,rhs) |..> xorRules
    | NodeCons -> (syms,spLst,lhs,rhs) |..> nodeConsRules
    | Is -> isRules syms spLst lhs rhs
    | Has -> hasRules syms spLst lhs rhs
    | Pipe -> (syms,spLst,lhs,rhs) |..> pipeRules
    | Collect -> (syms,spLst,lhs,rhs) |..> collectRules
    | Enpipe -> enpipeRules syms spLst lhs rhs
    | MulApp -> (syms,spLst,lhs,rhs) |..> mulAppRules
    | _ ->
        "Other binary operators"
        |> notImplementedYet

and evalPrefixExpr syms spLst e op =
    match op with
    | Not -> (syms,spLst,e) |.> notRules
    | Amp -> (syms,spLst,e) |.> ampRules
    | DblAmp -> (syms,spLst,e) |.> dblAmpRules
    | Dollar _ -> "Once application" |> notImplementedYet

and evalDot syms spLst lhs rhs =
    let lSpace = evalExpr syms spLst lhs
    match lSpace with
    | Error e -> e |> Error
    | Ok (Space(spType, syms')) -> 
        evalExpr [syms'] [] rhs
    // TODO: Move load forwarding to match statement evaluation
    // | Ok (Value (Node n)) ->
    //     match n.load with
    //     | Space (spType, syms') ->
    //         evalExpr [syms'] [] rhs
    //     | _ ->
    //         "The load of this node is not an object type."
    //         |> TypeError
    //         |> Error        
    | _ ->
        "Clearly this isn't an object or a namespace..."
        |> TypeError
        |> Error

and evalSExpr syms spLst s : BilboResult<Meaning> =
    match s with
    | Literal l -> evalLiteral l
    | ObjExpr oe -> evalObjExpr syms spLst oe
    | TypeCast(t,e) -> evalTypeCast syms spLst t e
    | SExpr.ParamList (eLst) ->
        let meanLst = List.map (evalExpr syms spLst) eLst
        let combineMeanLst (resLst : BilboResult<Meaning list>) (res : BilboResult<Meaning>) : BilboResult<Meaning list> = 
            match res, resLst with
            | Ok mean, Ok lst -> (mean::lst) |> Ok
            | Error e, _ -> e |> Error
            | _, Error e -> e |> Error
        let start : BilboResult<Meaning list> = Ok []
        meanLst
        |> List.rev
        |> List.fold combineMeanLst start
        |> function
        | Ok lst -> lst |> ParamList |> Ok
        | Error e -> e |> Error

and getNextParam pLine =
    match pLine with
    | PStage (Transform(tDef,_)) ->
        let tName, tParams, _, _ = tDef
        match tParams with
        | [] -> zeroParamTransformError()
        | next :: _ -> next |> Ok
    | PStage _ -> functionAlapError()
    | ThenPipe (pl1,pl2) -> getNextParam pl1
    | Modified (pl,_) -> getNextParam pl
    | OrPipe (pl1,pl2) -> "Or pipes" |> notImplementedYet     

and evalPostfixExpr syms spLst e op =
    let modifiedPLine = Modified >> Pipeline >> Value >> Ok
    let eRes = evalExpr syms spLst e
    match eRes with
    | Error e -> e |> Error
    | Ok (Value(Pipeline p)) ->
        match op with
        | AlapApp ->
            let alapParam = getNextParam p
            Result.bind (fun param -> (p, Alap param) |> modifiedPLine) alapParam
        | MaybeApp -> (p, Maybe) |> modifiedPLine
    | Ok m -> m |> typeStr |> alapMaybeAppNonPLine        

and evalExpr (syms : Symbols) spLst (e : Expr) : BilboResult<Meaning> =
    match e with
    | Var v -> Symbols.find syms {spLst=spLst; id=v}
    | SExpr s -> evalSExpr syms spLst s
    | BinExpr (lhs, Dot, rhs) -> evalDot syms spLst lhs rhs
    | BinExpr (lhs,op,rhs) -> evalBinExpr syms spLst lhs op rhs
    | GExpr g -> evalGExpr syms spLst g
    | PostfixExpr (e',op) -> evalPostfixExpr syms spLst e' op
    | PrefixExpr (op, e') -> evalPrefixExpr syms spLst e' op
    | SpecialExpr _ -> "Special expr [+] and [-]" |> notImplementedYet

and consVid syms spLst e : BilboResult<ValueId> =
    match e with
    | Var v -> {spLst=spLst; id=v} |> Ok
    | BinExpr(l, Dot, r) ->
        let lVid = consVid syms spLst l
        match lVid with
        | Error e -> e |> Error
        | Ok lVid' ->
            let rVid = consVid syms spLst r
            match rVid with
            | Error e -> e |> Error
            | Ok rVid' ->
                {rVid' with spLst=lVid'.spLst @ [Name lVid'.id]} |> Ok
    | _ ->
        "vId construction"
        |> notImplementedYet

and evalExprStatement (syms : Symbols) spLst (e : ExprStatement) : BilboResult<Symbols> =
    match e with
    | AssignmentExpr (eLhs, eRhs) ->
        let lhsId = consVid syms spLst eLhs
        match lhsId with
        | Error e -> e |> Error
        | Ok vid ->
            let rhs = evalExpr syms spLst eRhs
            match rhs with
            | Error e -> e |> Error
            | Ok (ParamList _) -> paramListTypeError "an identifier"
            | Ok rhsVal ->
                Symbols.set syms vid rhsVal
    | DeleteExpr id ->
        Symbols.remove syms {id=id; spLst=spLst}
    | PrintExpr (thing,place) ->
        match place with
        | None ->
            let strRes =         
                thing
                |> evalExpr syms spLst
                |> Result.bind print
            match strRes with
            | Error e -> e |> Error
            | Ok str ->
                printfn "%s" str
                syms |> Ok          
        | Some _ -> "Printing to files" |> notImplementedYet