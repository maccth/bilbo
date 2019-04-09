module Bilbo.SemanticAnalyser.SemanticAnalyser

open Bilbo.Parser.Ast
open Bilbo.SemanticAnalyser.SymbolTable

// TODO: Create Bilbo error types
type BilboError = string
type Analysed<'T> = Result<SymbolTable * 'T Option, BilboError>

let analysedBind (binder : 'T1 -> 'T2) (r : Analysed<'T1>) =
    match r with
    | Error e -> Error e
    | Ok(st, None) -> Ok(st,None)
    | Ok(st, Some astPart) -> 
        let astPart' = astPart |> binder |> Some
        Ok(st, astPart')

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

let aTypeDef st tdef nspace : Analysed<TypeDef> = 
    let tname = fst tdef
    match Map.containsKey (nspace, tname) st.types with
    | true -> "A type definition with this name already exists" |> Error
    | false ->
        let st' = addType st (nspace, tname) (tdef)
        (st', None) |> Ok

let aTransformDef (st : SymbolTable) tdef nspace : Analysed<TransformDef> =
    let tname,_,_,_ = tdef
    match Map.containsKey (nspace, tname) st.transforms with
    | true -> "A transform with this name already exists" |> Error
    | false ->
        let st' = addTransform st (nspace, tname) (tdef)
        (st', None) |> Ok

let rec checkAssignmentLhs e =
    match e with
    | Var id -> true
    | BinExpr (l, Dot, r) ->
        (checkAssignmentLhs l) && (checkAssignmentLhs r)
    | _ -> false

let aExprStatement st e : Analysed<ExprStatement> =
    match e with
    | AssignmentExpr (lhs, rhs) ->
        match checkAssignmentLhs lhs with
        | true -> Ok(st, Some e)
        | false -> "The left-hand side of this expression cannot be assigned to" |> Error
    | _ -> Ok(st, Some e)
        
let aImport st (fp,n) nspace : Analysed<FilePath * string> =
    let st' = addNamespace st n fp
    (st', None) |> Ok

let aProgramUnit st (pu : ProgramUnit) : Analysed<ProgramUnit> = 
    let (nspace, s) = pu
    let nspace' = nstr nspace
    match s with
    | TypeDef def ->
        aTypeDef st def nspace'
        |> analysedBind (fun t -> (nspace, TypeDef t) |> ProgramUnit)
    | TransformDef def ->
        aTransformDef st def nspace'
        |> analysedBind (fun t -> (nspace, TransformDef t) |> ProgramUnit)
    | ExprStatement e ->
        aExprStatement st e
        |> analysedBind (fun t -> (nspace, ExprStatement t) |> ProgramUnit)
    | Import(fp,n) ->
        aImport st (fp,n) nspace'
        |> analysedBind (fun t -> (nspace, Import t) |> ProgramUnit)
    
let rec aProgram (st : SymbolTable) astIn astOut : Analysed<Program> =
    match astIn with
    | line :: rest ->
        match aProgramUnit st line with
        | Ok (st', Some line') ->
            aProgram st' rest (line' :: astOut)
        | Ok (st', None) ->
            aProgram st' rest astOut
        | Error e ->
            Error e
    | [] ->
        let astOut' = List.rev astOut
        Ok(st, Some astOut')

let bilboSemanticAnalyser (astIn : Program) =
    aProgram emptySymbolTable astIn []

let bilboSemanticAnalyserPrint (astIn : Program) =
    let out = bilboSemanticAnalyser astIn
    match out with
    | Ok(st, Some ast) ->
        printfn "Program AST:"
        printfn "%A" ast
        printfn "Program symbol table:"
        printfn "%A" st
    | Error(s) ->
        printfn "%A" s
    | Ok(st, None) ->
        printfn "%s" "Passed semantic analysis but did not return an AST."
        printfn "%s" "The symbol table is:"
        printfn "%A" st
        failwithf "%s" "Will not happen. Program cannot parse and pass semantic analysis but return no AST."