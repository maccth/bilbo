module Bilbo.Evaluator.ExpressionStatement

open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Common.Error
open Bilbo.Evaluator.PrimativeTypes
open Bilbo.Evaluator.BinaryExpressions
open Bilbo.Evaluator.Print
open Bilbo.Graph.Graph

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
    let ops = evalBinOperands syms spLst lhs rhs
    match ops with
    | Ok (l, r) -> opRule (l,r)
    | Error e -> e |> Error

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
    | Enpipe -> enpipeRules syms spLst lhs rhs
    | _ ->
        "Other binary operators"
        |> notImplementedYet

and evalPStageBody (syms : Symbols) spLst st bod =
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

and evalMatchStatement (syms : Symbols) spLst tst mat : BilboResult<Meaning> =
    10 |> Int |> Value |> Ok

and applyArgToPipeline syms spLst (pLine : Pipeline) (param : Meaning) =
    match pLine with
    // | ParamStage ps :: pLine'->
    //     let ps' = param :: ps
    //     applyArgsToPipeline syms spLst pLine' ps'
    | [] -> param |> Ok
    | Transform(tDef, tst) :: pLineRest ->
        // "Applying arg to transform pipelines" |> notImplementedYet
        let tName,tParams,preMatchBod,tMatch = tDef
        match tParams with
        // | [] -> zeroParamFunctionError()
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
                        let res = evalMatchStatement syms' spLst tst tMatch
                        Result.bind (applyArgToPipeline syms spLst pLineRest) res
                // | _ ->
                //     // NOTE: not pure implementation of currying. 
                //     // We don't move on in the pipeline until we the funciton has all its args)
                //     ((fId, paramsLeft, bod, ret), fstUpdated)
                //     |> Function
                //     |> fun p -> p :: pLineRest
                //     |> Pipeline
                //     |> Value
                //     |> Ok
                    // let funcAsParam = ^^^
                    // applyArgToPipeline syms spLst pLine' funcAsParam
    | Function(fDef, fst) :: pLineRest ->
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
                    match funcResultAsParam with
                    | Error e -> e |> Error
                    | Ok param' -> applyArgToPipeline syms spLst pLineRest param'
                | _ ->
                    // NOTE: not pure implementation of currying. 
                    // We don't move on in the pipeline until we the funciton has all its args)
                    ((fId, paramsLeft, bod, ret), fstUpdated)
                    |> Function
                    |> fun p -> p :: pLineRest
                    |> Pipeline
                    |> Value
                    |> Ok
                    // let funcAsParam = ^^^
                    // applyArgToPipeline syms spLst pLine' funcAsParam

and applyArgsToPipeline syms spLst pLine args =
    match args with
    | [] -> "Control should have passed onto single arg handler." |> ImplementationError |> Error
    | [arg] ->
        applyArgToPipeline syms spLst pLine arg
    | arg :: rest ->
        let evalRes = applyArgToPipeline syms spLst pLine arg
        match evalRes with
        | Error e -> e |> Error
        | Ok (Value(Pipeline(pLine'))) -> applyArgsToPipeline syms spLst pLine' rest
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
        | ParamList(pLst), Value (Pipeline pLine) -> applyArgsToPipeline syms spLst pLine pLst
        | _, Value (Pipeline pLine) -> applyArgToPipeline syms spLst pLine lMean
        | _ ->
            "The enpipe operator requires a function, transform or pipeline on the right-hand side."
            |> TypeError
            |> Error

and varAsString var =
    match var with
    | Var(str) -> str |> Some 
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

and evalEdgeOp syms spLst (lhs : Node) (rhs : Node) (eOp : EdgeOp) =
    let w : BilboResult<EdgeWeight> =
        match eOp with
        | Left None | Right None | Bidir None -> None |> Ok
        | Left (Some(we))
        | Right (Some(we))
        | Bidir (Some(we)) ->
            let wRes = evalExpr syms spLst we
            match wRes with
            | Error e -> e |> Error
            | Ok w -> w |> Some |> Ok
    match w,eOp with
    | Error e, _ -> e |> Error
    | Ok w', Left _ -> [{Edge.source=rhs; target=lhs; weight=w'}] |> Ok
    | Ok w', Right _ -> [{Edge.source=lhs; target=rhs; weight=w'}] |> Ok
    | Ok w', Bidir _ -> [{Edge.source=lhs; target=rhs; weight=w'}; {source=rhs; target=lhs; weight=w'}] |> Ok

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
                    | PathElem.Node e ->
                        let n = evalExpr syms spLst e
                        match n with
                        | Ok (Value(Node n')) -> Graph.addNode n' g'
            peLst
            |> List.fold pathScan (Ok Graph.empty)
            |> Result.bind (Graph >> Value >> Ok)

and evalExpr (syms : Symbols) spLst (e : Expr) : BilboResult<Meaning> =
    match e with
    | Var v -> Symbols.find syms {spLst=spLst; id=v}
    | BinExpr (lhs, Dot, rhs) -> evalDot syms spLst lhs rhs
    | SExpr s -> evalSExpr syms spLst s
    | BinExpr (lhs,op,rhs) -> evalBinExpr syms spLst lhs op rhs
    | GExpr g -> evalGExpr syms spLst g
    | _ ->
        "Other expression types"
        |> notImplementedYet

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