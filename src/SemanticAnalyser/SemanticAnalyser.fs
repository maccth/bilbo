module Bilbo.SemanticAnalyser.SemanticAnalyser

open Bilbo.Parser.Ast
open Bilbo.SemanticAnalyser.Value
// open Bilbo.Parser.Parser

let checkLhsAssign s =
    let rec checkLhsExpr lhs =
        match lhs with
        | Var _id -> true
        | BinExpr (l, Dot, r) ->
            (checkLhsExpr l) && (checkLhsExpr r)
        | _ -> false
    match s with
    | ExprStatement (AssignmentExpr(lhs,rhs)) -> checkLhsExpr lhs
    | _ -> true

type SymbolTable = {
    types       : Map<string * string, TypeDef>; 
    transforms  : Map<string * string, TransformDef>;
    variables   : Map<string * string, Value>;
    namespaces  : Map<string, Value>;
}

let consSymbolTable = {
    types       = Map.empty;
    transforms  = Map.empty;
    variables   = Map.empty;
    namespaces  = Map.empty;
}

let addType (st : SymbolTable) (nspace,tname) tdef =
    {st with types = Map.add (nspace,tname) tdef st.types}

// TODO: Create Bilbo error types
type BilboError = string
//                  Result<(SymbolTable * 'T option), BilboError>)
type Analysis<'T> = Result<SymbolTable * 'T Option, BilboError>

let analysisBind (binder : 'T1 -> 'T2) (r : Analysis<'T1>) =
    match r with
    | Error e -> Error e
    | Ok(st, None) -> Ok(st,None)
    | Ok(st, Some astPart) -> 
        let astPart' = astPart |> binder |> Some
        Ok(st, astPart')

// type Analyser<'T> = SymbolTable -> 'T -> Analysis<'T>

let nstr (nlst : Namespace list) =
    let str e =
        match e with
        // The symbol `#` cannot appear in namespace identifiers so this provides 
        //  a distinction between the TOP level namespace and namespaces called "TOP".
        // Furthermore, the symbol `.` cannot appear in namespace identifiers.
        | Top  -> "#Top"
        | Name s -> s
    let rec nstr' nlst res =
        match nlst with
        | [n] -> res + (str n)
        | n :: nlst' -> nstr' nlst' (res + (str n) + ".") 
        | [] -> res
    nstr' (List.rev nlst) ""

let aTypeDef st tdef nspace : Analysis<TypeDef> = 
    let tname, attrs = tdef
    match Map.containsKey (nspace, tname) st.types with
    | true -> "Type definition with this name already exists" |> Error
    | false ->
        let st' = addType st (nspace, tname) (tname,attrs)
        (st', None) |> Ok

// TODO: requires expression semantic analysis
// let aTransformDef st tdef nspace =

let aExprStatement st e nspace : Analysis<ExprStatement> =
    (st, Some e) |> Ok

let aProgramUnit st (pu : ProgramUnit) : Analysis<ProgramUnit> = 
    let (nspace, s) = pu
    let nspace' = nstr nspace
    match s with
    | TypeDef def ->
        aTypeDef st def nspace'
        |> analysisBind (fun t -> (nspace, TypeDef t) |> ProgramUnit)
    // | TransformDef def ->
    | ExprStatement e ->
        aExprStatement st e nspace'
        |> analysisBind (fun t -> (nspace, ExprStatement t) |> ProgramUnit)
    // Import statements are handled at parse time
    | Import _ -> (st, None) |> Ok
    | _ -> (st, Some pu) |> Ok
    
let rec aProgram (st : SymbolTable) (astIn : Program) (astOut : Program) =
    match astIn with
    | line :: rest ->
        match aProgramUnit st line with
        | Ok (st', Some line') ->
            aProgram st' rest (line' :: astOut)
        | Ok (st', None) ->
            aProgram st' rest astOut
        | Error e ->
            printfn "%s" e
    | [] ->
        let astOut' = List.rev astOut
        printfn "%A" astOut'
        printfn "%A" st

let analyseSemanticsTop astIn =
    aProgram consSymbolTable astIn [] 






