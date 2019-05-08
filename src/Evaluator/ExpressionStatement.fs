module Bilbo.Evaluator.ExpressionStatement

open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Common.Error
open Bilbo.Evaluator.PrimativeTypes
open Bilbo.Evaluator.BinaryExpressions
open Bilbo.Common

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
    | Pipe -> pipeRules syms spLst lhs rhs
    // | Pipe -> (syms,spLst,lhs,rhs)  
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error

and evalFuncBody (syms : Symbols) spLst fst bod ret =
    let syms' : Symbols = fst :: syms
    let folder syms (es : ExprStatementL) =
        match syms with
        | Error e -> e |> Error
        | Ok syms' ->
            let loc,es' = es
            evalExprStatement syms' spLst es'
    let symsPostFunc = List.fold folder (Ok syms') bod
    match symsPostFunc with
    | Error e -> e |> Error
    | Ok syms'' -> evalExpr syms'' spLst ret

and pipeParam syms spLst (fDef : FunctionDef) fst param = 
    let fId, fParams, bod, ret = fDef
    match fParams with
    | hd :: paramsLeft ->
        let fst' = SymbolTable.set fst {id=hd; spLst=[]} param
        match fst' with
        | Error e -> e |> Error
        | Ok fst'' ->
            match paramsLeft with
            // | [] -> evalFuncBody syms spLst fst bod ret
            | [] -> evalFuncBody syms [] fst'' bod ret
            | _ ->
                ((fId,paramsLeft,bod,ret), fst'')
                |> Function
                |> fun p -> [p]
                |> Pipeline
                |> Value
                |> Ok

and pipeRules (syms : Symbols) spLst (l : Expr) (r : Expr) : BilboResult<Meaning> =
    let lMean = evalExpr syms spLst l
    let rMean = evalExpr syms spLst r
    match lMean,rMean with
    | Error e, _
    | _, Error e -> e |> Error
    | Ok l, Ok r ->
        match r with
        | Value (Pipeline p) ->
            match p with
            | Function (fDef,fst) :: rest ->
                let appliedFunc = pipeParam syms spLst fDef fst l
                match appliedFunc with
                | Error e -> e |> Error
                | Ok(Value(Pipeline([p]))) -> p :: rest |> Pipeline |> Value |> Ok
                | Ok(m) -> m |> Ok

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
                // TODO: implementation for graphs, type defs, transform defs, nodes...
                | String _ -> "str" |> Ok
                | Float _ -> "float" |> Ok
                | Int _ -> "int" |> Ok
                | Bool _ -> "bool" |> Ok
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

and evalObjExpr syms spLst typ attrs : BilboResult<Meaning> =
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
    | _ ->
        "Type " + "\"" + typ + "\" is not defined."
        |> NameError
        |> Error

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
    // | SExpr.Unit -> Unit |> Value |> Ok
    | Literal l -> evalLiteral l
    | ObjExpr(t,attrs) -> evalObjExpr syms spLst t attrs
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

and evalExpr (syms : Symbols) spLst (e : Expr) : BilboResult<Meaning> =
    match e with
    | Var v -> Symbols.find syms {spLst=spLst; id=v}
    | BinExpr (lhs, Dot, rhs) -> evalDot syms spLst lhs rhs
    | SExpr s -> evalSExpr syms spLst s
    | BinExpr (lhs,op,rhs) -> evalBinExpr syms spLst lhs op rhs 
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error

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
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error
    
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
            | Ok (ParamList _) ->
                "Cannot bind a parameter list to an identifier"
                |> TypeError
                |> Error
            | Ok rhsVal ->
                Symbols.set syms vid rhsVal
    | PrintExpr (_) ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError 
        |> Error