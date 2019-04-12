module Bilbo.Parser.Parser

open FParsec
open Bilbo.Common.Ast
open Bilbo.Common.Error

let qp x = printfn "%A" x

// FParsec line and column number
// ==============================
// credit : https://stackoverflow.com/questions/55590902/fparsec-keeping-line-and-column-numbers
// edited by maccth

type WithPos<'T> = (Loc * 'T)

module Position =
    /// Get the previous position on the same line.
    let leftOf (p: Position) =
        if p.Column > 1L then
            Position(p.StreamName, p.Index - 1L, p.Line, p.Column - 1L)
        else
            p

    /// Wrap a parser to include the position
    let attachPos fname (p: Parser<'T, 'U>) : Parser<WithPos<'T>, 'U> =
        // Get the position before and after parsing
        pipe3 getPosition p getPosition <| fun start value finish ->
            let loc = {
                file = fname;
                startLine = start.Line;
                startCol = start.Column;
                endLine = (leftOf finish).Line;
                endCol = (leftOf finish).Column;
            }
            (loc, value)

    let bind binder (p : Parser<WithPos<'T>, 'U>) =
        p |>> binder

// Extensions to FParsec
// =====================
let commentLine =
    skipString "//" .>>. skipManyTill anyChar newline

let commentBlock =
    skipString "/*" .>>. skipManyTill anyChar (skipString "*/")

let comment =
    commentLine <|> commentBlock
    |>> ignore
    |> fun p -> p <?> "comment"

let ws = (many (choice [spaces1; comment;]) |>> ignore) <?> "whitespace"

let str s = pstring s .>> ws
// Identical to `str` but increases readability by distinguishing between strings and keywords
let keyw = str

/// Exact version of str. `stre s` parses the string `s` with no spaces after.
let stre s = pstring s

let tuple6 a b c d e f =
    pipe2 (tuple5 a b c d e) f
        (fun (a',b',c',d',e') f' -> a',b',c',d',e',f')

let pipe6 a b c d e f func =
    tuple6 a b c d e f
    |>> func

/// `chance [p1; p2; p3]` is equivalent to `choice [attempt p1; attempt p2; p3]`
let chance pLst =
    match List.rev pLst with
    | [] -> []
    | last :: rest ->
        last :: (List.map attempt rest)
        |> List.rev
    |> choice

/// Parses `p` between `(` and `)` with white space consumed after either bracket
let brackets p = between (str "(") (str ")") p

/// `csv p` is equivalent to `sepBy p (str ",")`
let csv p = sepBy p (str ",") 
/// `csv1 p` is equivalent to `sepBy1 p (str ",")`
let csv1 p = sepBy1 p (str ",")
let csv2 p =
    let com = str ","
    pipe3 p com (csv1 p) (fun hd _ rest -> hd::rest)

// Parser
// ======
let keywords = 
    [
        "type";
        "def";
        "return"; "become";
        "is"; "and"; "not";
        "match";
        "where";
        "True"; "False";
        "print";
        "import";
    ] |> Set.ofList

let pnKeyword : Parser<unit, unit> =
    let kws = Set.toList keywords
    let pKws = kws |> List.map str |> List.toSeq 
    notFollowedBy (choice pKws)

let pId : Parser<string, unit> =
    let firstChar c = isLetter c
    let middleChar c = firstChar c || c = '_' || isDigit c
    let upToLastChar = many1Satisfy2 firstChar middleChar
    let lastChar = manyChars (pchar ''') .>> ws
    let specialVars =
        [
            "(++)"; // For the positive and negative application conditions in a match
            "(--)";
        ] |> List.map str |> choice
    let p = specialVars <|> (pnKeyword >>. (pipe2 upToLastChar lastChar (+)))
    p <?> "variable identifier"

let pVar = pId |>> Var

let pStr : Parser<string, unit> =
    let chars = manySatisfy (fun c -> c <> '"')
    between (stre "\"") (str "\"") chars

let pStrLit =
    pStr |>> StrLit

let pIntLit : Parser<Literal, unit> =
    pint32 .>> notFollowedBy (stre ".") .>> ws |>> int |>> IntLit

let pFloatLit : Parser<Literal, unit> =
    pfloat .>> ws |>> FloatLit

let pBoolLit : Parser<Literal, unit> =
    (str "True" <|> str "False")
    |>> function
        | t when t="True" -> true
        | _ -> false
    |>> BoolLit

let pLiteral =
    let p = choice [pBoolLit; pStrLit; attempt pIntLit; pFloatLit] 
    p <?> "literal" |>> Literal |>> SExpr 

let exprOpp = new OperatorPrecedenceParser<Expr,unit,unit>()
let pExpr = exprOpp.ExpressionParser

let pExprTerm, pExprTermRef = createParserForwardedToRef()
exprOpp.TermParser <- pExprTerm <|> brackets pExpr;

let pObjInstan' =
    let attrs = (csv pExpr) |> brackets
    let consObj aLst tName = (tName, aLst) |> ObjInstan |> ObjExpr |> SExpr
    attrs |>> consObj

// TODO: potentially add object field indexing obj["field1"]
let postIds = pObjInstan'

// Parses things that appear after IDs, object instantiation (MyT(p1,p2,p3)) and dot access (myObj.a)
let pPostIds =
    let checkPosts s p =
        match p with
        | Some p' -> p' s
        | None -> s |> Var
    pipe2 pId (opt postIds) checkPosts

let pParamList = pExpr |> csv2 |> brackets |>> ParamList |>> SExpr

let pSExpr = choice [pParamList; pPostIds; pLiteral;]

// Vaguely based on Python 3 operator precedence
// https://docs.python.org/3/reference/expressions.html#operator-precedence
let binExprOps1 =
    let al = Associativity.Left
    let ar = Associativity.Right
    [
        "::", 12, al, NodeCons;
        ".", 11, al, Dot

        "^", 10, ar, Pow;

        "**", 9, al, MulApp;
        "*!*", 9, al, UpToApp;
        // Times is defined below
        "/", 9, al, Divide;

        "+", 8, al, Plus;
        // Minus is defined below

        "<", 7, al, LessThan;
        "<=", 7, al, LessThanEq;
        // Greater than is defined below
        ">=", 7, al, GreaterThanEq;
        "==", 7, al, Equal;
        "is", 7, al, Is;
        "!=", 7, al, NotEqual;

        // "not" has precedence 6, but is prefix (unary)
        "and", 5, al, And;
        "or", 4, al, Or;

        "<|>", 3, al, OrPipe;
        "|>", 2, al, Pipe;
    ] |> List.map (fun (op, prec, assoc, astOp) -> (op, prec, None, assoc, astOp))

let binExprOps2 =
    let al = Associativity.Left
    [
        // Stops conflicts with `**` and `*!*`
        "*", 9, (str "*") <|> (str "!") , al, Times;
        // Stops conflicts with `->`
        "-", 8, str ">", al, Minus;
        // Stops conflicts with `x>,y` in path expressions and `[a>:b,>,c]` in path comprehensions
        ">", 7, (str ",") <|> (str ":"), al, GreaterThan;
    ] |> List.map (fun (op, prec, nf, assoc, astOp) -> (op, prec, Some nf, assoc, astOp))

exprOpp.AddOperator(PrefixOperator("&&", ws, 11, true, fun x -> (DblAmp,x) |> PrefixExpr ))
exprOpp.AddOperator(PrefixOperator("&", (notFollowedBy (str "&")) >>. ws, 11, true, fun x -> (Amp,x) |> PrefixExpr ))
exprOpp.AddOperator(PrefixOperator("not", ws, 8, true, fun x -> (Not,x) |> PrefixExpr))

exprOpp.AddOperator(PrefixOperator("$", ws, 11 , true, fun x -> (Dollar,x) |> PrefixExpr))
exprOpp.AddOperator(PostfixOperator("!", ws, 10, true, fun x -> (x, ALAPApp) |> PostfixExpr ))
exprOpp.AddOperator(PostfixOperator("?", ws, 10, true, fun x -> (x, MaybeApp) |> PostfixExpr))

let binExprOps = List.append binExprOps1 binExprOps2

let addBinOp (op, prec, nf, assoc, astOp) =
    let cons op l r = (l,op,r) |> BinExpr 
    let inOp =
        match nf with
        | Some nf' ->
            InfixOperator(op, notFollowedBy (nf') >>. ws, prec, assoc, (astOp |> cons))
        | None ->
            InfixOperator(op, ws, prec, assoc, (astOp |> cons))
    exprOpp.AddOperator(inOp)

List.map addBinOp binExprOps |> ignore

let weight = pExpr

/// Left and bidirectional edge operators `<`, `<e`, `<>` `<e>`
let pLeftEdgeOps =
    tuple3 (str "<") (opt weight) (opt (str ">"))
    |>> function
    | l, e, Some r -> e |> Bidir
    | l, e, None -> e |> Left

/// Right edge operators `>`, `e>`
let pRightEdgeOps =
    tuple2 (opt weight) (str ">") |>> fst |>> Right

let pEdgeOp = (pLeftEdgeOps <|> pRightEdgeOps) <?> "edge"

let pNodeExpr = pExpr <?> "node"

let pPathExpr =
    let edge = pEdgeOp .>> followedBy (str (",") .>>. pNodeExpr)
    let commaEdge = str "," >>. edge
    let pathElem = pNodeExpr .>>. opt (pipe2 (followedBy commaEdge) (commaEdge) (fun x y -> y))
    let compOp =
        let setId = pipe2 ((notFollowedBy (str "&&")) >>. str "&") (str "=") (fun _ _ -> SetId)
        let setLoad = pipe3 (str "&&") (str "=") pExpr (fun _ _ e -> e |> SetLoad)
        let addEdge = pEdgeOp |>> AddEdge
        choice [setId; setLoad; addEdge]
    let compOps =
        pipe3 (followedBy ((csv1 compOp) .>>. str ":")) (csv1 compOp) (str ":") <|
            fun _ compOps' _ -> compOps'
    let path =
         (opt compOps .>>. csv pathElem) |> between (str "[") (str "]")
           
    let consPathElem (elems,prevEdge) (el : Expr * EdgeOp Option) =
        // n2 <--prevEdge--> n1 <--nextEdge--> 
        // prevEdge is the incoming edge operator from n1 to n1
        // nextEdge is the outgoing edge operator from n1
        let n1, nextEdge = el
        match prevEdge with
        | Some e ->
            match elems with
            | Node n2 :: elems' -> (((n2,e,n1) |> Edge) :: elems'), nextEdge
            | Edge (_,_,n2) :: elems' -> (((n2,e,n1) |> Edge) :: elems), nextEdge
            | [] -> failwith "Will not happen. First element cannot have incoming edge."
        | None -> ((n1 |> Node) :: elems), nextEdge

    let consPath p =
        List.fold consPathElem ([],None) p
        |> function
        | (lst, None) -> lst
        | (lst, Some e) -> failwith "Will not happen. Final element cannot have outgoing edge."
        |> List.rev

    path
    |>> function
        | Some compOps, elems ->
            (compOps, (consPath elems))
            |> PathComp
        | None, elems ->
            elems 
            |> consPath
            |> Path
    |>> PathExpr
    |>> GExpr
    |> fun p -> p <?> "path expression"

let pGExpr = pPathExpr

let exprs = [pSExpr; pVar; pGExpr;]
do pExprTermRef := chance exprs
   
let pAssignmentExpr =
    let ctor var _ expr  =  (var, expr) |> AssignmentExpr
    let p = pipe3 pExpr (str "=") pExpr ctor
    p <?> "assignment expression"

let pPrintExpr =
    let dest = keyw "to" >>. pExpr
    pipe2 (keyw "print" >>. pExpr) (opt dest) (fun e d -> (e,d) |> PrintExpr)

let pExprStatementL fname =
    choice [pAssignmentExpr; pPrintExpr]
    |> Position.attachPos fname

let pReturn = keyw "return" >>. pExpr |>> Return
let pBecome = keyw "become" >>. pExpr |>> Become
let pTerminatingStatement =
    (pReturn <|> pBecome)
    |> fun p -> p <?> "terminating statement (`return` or `become`)" 

let pMatchCase fname =
    let cons lhs where _arrow body term =
        match lhs with
        | Var v when v="_" ->
            (where,body,term) |> CatchAll
        | _ ->
            (lhs, where,body,term) |> Case
    let body = many (pExprStatementL fname)
    let whereClause = (keyw "where") >>. many pExpr
    let matcher = (str ("_") |>> Var) <|> pExpr
    let p = pipe5 matcher (opt whereClause) (str "->") body pTerminatingStatement cons
    p <?> "match case"

let pMatchStatement fname =
    let cases = (str "|") >>. sepBy1 (pMatchCase fname) (str "|")
    let p = pipe3 (keyw "match") (opt pExpr) cases (fun _m e c -> (e,c) |> MatchStatement)
    p <?> "match statement"

let pTransformDef fname =
    let paramCsv = csv pId
    let paramBrac = brackets paramCsv <|> paramCsv 
    let exprs = many (pExprStatementL fname)
    let matches = pMatchStatement fname
    let cons (def, tName, paramLst, eq, exprs, matches) =
        (tName, paramLst, exprs, matches) |> TransformDef
    let p = pipe6 (keyw "def") pId paramBrac (str "=") exprs matches cons
    let p' = p <?> "transform definition"
    p'
    |> Position.attachPos fname
    |> Position.bind TransformDefL

let pTypeDef fname =
    let csvIds1 = csv1 pId
    let csvIds = csv pId
    let bracIds = brackets csvIds
    let ctor = fun _ name _ attrs -> (name, attrs) |> TypeDef
    let p = pipe4 (keyw "type") (pId <?> "type name") (str "=") (bracIds <|> csvIds1) ctor
    let p'= p <?> "type definition"
    p'
    |> Position.attachPos fname
    |> Position.bind TypeDefL

let pImport fname =
    let file = keyw "import" >>. pStr
    let nspace = keyw "as" >>. pId
    let p = file .>>. nspace |>> Import
    let p' = p <?> "import"
    p'
    |> Position.attachPos fname
    |> Position.bind ImportL

let pStatement fname =
    let pe fname = fname |>  pExprStatementL |>> ExprStatementL
    let ps = [pe; pTypeDef; pTransformDef; pImport;]
    let ps' = List.map (fun p -> p fname) ps
    choice ps'
    
let pProgramUnit nspace fname =
    pStatement fname
    |>> fun s ->
        (nspace, s) |> ProgramUnit

let pProgram nspace fname = ws >>. choice [pProgramUnit nspace fname] |> many1 .>> ws

let pFile nspace fname = (pProgram nspace fname) .>> eof

// Top level parsers
// =================
let pBilboFile' nspace file =
    runParserOnFile (pFile nspace file) () file System.Text.Encoding.UTF8

let pBilboStr' nspace str =
    let stream = "user input"
    runParserOnString (pFile nspace stream) () stream  str 

let getAst reply fSucc fFail : BilboResult<Program>=
    match reply with
    | Success(ast, _s, _p) ->
        fSucc ast
    | Failure(msg, err, u) ->
        fFail msg err u

let rec resolveImports (astIn : ProgramUnit list) : BilboResult<Program> =
    let rec resolveImports' (astIn : ProgramUnit list) (astOut : ProgramUnit list) =
        match astIn with
        |  (nlst, ImportL (loc, (fp, nspace))) :: rest ->
            let importedLines = pBilboFile fp (Name nspace :: nlst)
            match importedLines with
            | FSharp.Core.Ok importedLines' -> 
                // Keep import statement for semantic analysis to record the namespace
                let importLine = (nlst, ImportL (loc, Import(fp, nspace)))
                // TODO: Construct AST in reverse by prepending and then reverse at the end
                let astOut' = List.append astOut (importLine :: importedLines')
                resolveImports' rest astOut'
            | FSharp.Core.Error e ->
                e |> FSharp.Core.Error
        | line :: rest ->
            resolveImports' (rest) (astOut @ [line])
        | [] -> astOut |> FSharp.Core.Ok
    resolveImports' astIn []

and pBilboFile file nspace : BilboResult<Program> =
    let res = pBilboFile' nspace file
    getAst res resolveImports (parseError file)

let bilboParser file : BilboResult<Program> =
    pBilboFile file [Top]

let bilboStringParser str =
    let res = pBilboStr' [Top] str   
    getAst res resolveImports (parseError "user input")

let bilboStringParserPrint str =
    let res = bilboStringParser str
    match res with
    | FSharp.Core.Ok ast ->
        printfn "%A" ast
    | FSharp.Core.Error err ->
        printfn "%A" err