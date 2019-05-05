module Bilbo.Evaluator.ExpressionStatement

open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Common.Error
open Bilbo.Evaluator.PrimativeTypes
open Bilbo.Evaluator.BinaryExpressions

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
    | Minus -> (syms,spLst,lhs,rhs) |..> minusRules
    | Times -> (syms,spLst,lhs,rhs) |..> timesRules
    | Divide -> (syms,spLst,lhs,rhs) |..> divideRules
    | Percent -> (syms,spLst,lhs,rhs) |..> moduloRules
    | Pow -> (syms,spLst,lhs,rhs) |..> powRules
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
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error

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
        match rhs with
        | Var(rTyp) -> rTyp |> Ok
        | BinExpr(_,Dot,_) ->
            let rhs' = evalExpr syms spLst rhs
            match rhs' with
            | Error e -> e |> Error
            | Ok (Value(Type(rTyp,_))) -> rTyp |> Ok
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
    | _ ->
        "Type " + "\"" + typ + "\" is not defined."
        |> NameError
        |> Error

and evalObjExpr syms spLst e : BilboResult<Meaning> =
    match e with
    | ObjInstan(typ, attrs) ->
        match typ with
        | "str"
        | "float"
        | "int"
        | "bool" -> typeCast syms spLst typ attrs
        | _ -> evalObjInstan syms spLst typ attrs

and evalDot syms spLst lhs rhs =
    let lSpace = evalExpr syms spLst lhs
    match lSpace with
    | Error e -> e |> Error
    | Ok (Space(spType, syms')) -> 
        evalExpr [syms'] [] rhs
    | Ok (Value (Node n)) ->
        match n.load with
        | Space (spType, syms') ->
            evalExpr [syms'] [] rhs
        | _ ->
            "The load of this node is not an object type."
            |> TypeError
            |> Error        
    | _ ->
        "Clearly this isn't an object or a namespace..."
        |> TypeError
        |> Error

and evalExpr (syms : Symbols) spLst e : BilboResult<Meaning> =
    match e with
    | Var v -> Symbols.find syms {spLst=spLst; id=v}
    | BinExpr (lhs, Dot, rhs) -> evalDot syms spLst lhs rhs
    | SExpr (Literal l) -> evalLiteral l
    | SExpr (ObjExpr o) -> evalObjExpr syms spLst o
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
    
let evalExprStatement (syms : Symbols) spLst (e : ExprStatement) : BilboResult<Symbols> =
    match e with
    | AssignmentExpr (eLhs, eRhs) ->
        let lhsId = consVid syms spLst eLhs
        match lhsId with
        | Error e -> e |> Error
        | Ok vid ->
            let rhs = evalExpr syms spLst eRhs
            match rhs with
            | Error e -> e |> Error
            | Ok rhsVal ->
                Symbols.set syms vid rhsVal
    | _ ->
        // TODO: Implement!
        "Not implemented yet."
        |> ImplementationError
        |> Error