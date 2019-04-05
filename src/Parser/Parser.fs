module Bilbo.Parser.Parser

open FParsec
open Ast
open Bilbo.Parser
open Bilbo.Parser.Ast

let qp x = printfn "%A" x

// Extensions to FParsec
// =====================
let ws = spaces

let str s = pstring s .>> ws

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
    ] |> Set.ofList

let pnKeyword : Parser<unit, unit> =
    let kws = Set.toList keywords
    let pKws = kws |> List.map str |> List.toSeq 
    notFollowedBy (choice pKws)

let pId : Parser<string, unit> =
    let firstChar c = isLetter c || c = '_'
    let middleChar c = firstChar c || isDigit c
    let upToLastChar = many1Satisfy2 firstChar middleChar
    let lastChar = manyChars (pchar ''') .>> ws
    // For the special `++` variable available in match cases
    (str "++") <|> (pnKeyword >>. (pipe2 upToLastChar lastChar (+)))

let pVar = pId |>> Var |>> VExpr

let pTypeDef =
    let csvIds1 = sepBy1 pId (str ",") 
    let csvIds = sepBy pId (str ",")
    let bracIds = brackets csvIds
    let ctor = fun _ name _ attrs -> (name, attrs) |> TypeDef
    pipe4 (str "type") pId (str "=") (bracIds <|> csvIds1) ctor

let pStrLit : Parser<Literal, unit> =
    let chars = manySatisfy (fun c -> c <> '"')
    between (stre "\"") (str "\"") chars |>> StrLit

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
    choice [pBoolLit; pStrLit; attempt pIntLit; pFloatLit] |>> Literal |>> SExpr 

let exprOpp = new OperatorPrecedenceParser<Expr,unit,unit>()
let pExprOpp = exprOpp.ExpressionParser

let pExpr, pExprRef = createParserForwardedToRef()
exprOpp.TermParser <- pExpr <|> brackets pExprOpp;

let pDotAccess' =
    let attrs = (str ".") >>. sepBy1 pId (str ".")
    let consDot aLst var = aLst |> List.fold (fun x y ->  (x, y) |> DotAccess |> VExpr) (var |> Var |> VExpr) 
    attrs |>> consDot

let pObjInstan' =
    let attrs = (sepBy pExprOpp (str ",")) |> brackets
    let consObj aLst tName = (tName, aLst) |> ObjInstan |> ObjExpr |> SExpr
    attrs |>> consObj

let postIds = choice [pDotAccess'; pObjInstan']

// Parses things that appear after IDs, object instantiation (MyT(p1,p2,p3)) and dot access (myObj.a)
let pPostIds =
    let checkPosts s p =
        match p with
        | Some p' -> p' s
        | None -> s |> Var |> VExpr
    pipe2 pId (opt postIds) checkPosts
    
// let sExprTerms = chance [pPostIds; pVar; pLiteral]

let pSExpr = choice [pPostIds; pLiteral;]

// sExprOpp.TermParser <- sExprTerms <|> brackets sExpr

// Vaguely based on Python 3 operator precedence
// https://docs.python.org/3/reference/expressions.html#operator-precedence
let binExprOps1 =
    let al = Associativity.Left
    let ar = Associativity.Right
    [
        // ".", 9, al, SBinOp.Dot
        "^", 8, ar, Pow;

        "**", 7, al, MulApp;
        "*!*", 7, al, UpToApp;
        // Times is defined below
        "/", 7, al, Divide;

        "+", 6, al, Plus;
        // Minus is defined below

        "<", 5, al, LessThan;
        "<=", 5, al, LessThanEq;
        // Greater than is defined below
        ">=", 5, al, GreaterThanEq;
        "==", 5, al, Equal;
        "is", 5, al, Is;
        "!=", 5, al, NotEqual;

        // "not" has precedence 4, but is prefix (unary)
        "and", 3, al, And;
        "or", 2, al, Or;

        "<|>", 3, al, OrPipe;
        "|>", 2, al, Pipe;
    ] |> List.map (fun (op, prec, assoc, astOp) -> (op, prec, None, assoc, astOp))

let binExprOps2 =
    let al = Associativity.Left
    [
        // Stops conflicts with `**` and `*!*`
        "*", 7, (str "*") <|> (str "!") , al, Times;
        // Stops conflicts with `->`
        "-", 6, str ">", al, Minus;
        // Stops conflicts with `x>,y` in path expressions
        ">", 5, str ",", al, GreaterThan;
    ] |> List.map (fun (op, prec, nf, assoc, astOp) -> (op, prec, Some nf, assoc, astOp))

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

exprOpp.AddOperator(PrefixOperator("&", ws, 9, true, fun x -> (Amp,x) |> PrefixExpr ))
exprOpp.AddOperator(PrefixOperator("not", ws, 3, true, fun x -> (Not,x) |> PrefixExpr))

exprOpp.AddOperator(PostfixOperator("!", ws, 8, true, fun x -> (x, ALAPApp) |> PostfixExpr ))
exprOpp.AddOperator(PostfixOperator("?", ws, 8, true, fun x -> (x, MaybeApp) |> PostfixExpr))
exprOpp.AddOperator(PrefixOperator("$", ws, 9, true, fun x -> (Dollar,x) |> PrefixExpr))

List.map addBinOp binExprOps |> ignore

// let pSExpr = sExpr

// TODO: Maybe pExpr is more appropriate here
let weight = pExprOpp

/// Left and bidirectional edge operators `<`, `<e`, `<>` `<e>`
let pLeftEdgeOps =
    tuple3 (str "<") (opt weight) (opt (str ">"))
    |>> function
    | l, e, Some r -> e |> Bidir
    | l, e, None -> e |> Left

/// Right edge operators `>`, `e>`
let pRightEdgeOps =
    tuple2 (opt weight) (str ">") |>> fst |>> Right

let pEdgeOp = (pLeftEdgeOps <|> pRightEdgeOps)

// Eliminate implicit left recursion for NodeCons expressions
let pExprNoNC, pExprNoNCRef = createParserForwardedToRef()

let pNodeCons =
    pipe3 pExprNoNC (str "::") pExprNoNC <| fun i _  l -> (i,l) |> NodeCons

let pNodeExpr =
    // TODO: Add transform application expressions
    chance [attempt pNodeCons; pVar]

let pPathExpr =
    let edge = pEdgeOp .>> followedBy (str (",") .>>. pNodeExpr)
    let commaEdge = str "," >>. edge
    let elem = pNodeExpr .>>. opt (pipe2 (followedBy commaEdge) (commaEdge) (fun x y -> y))
    let path = sepBy elem (str ",") |> between (str "[") (str "]")
    let folder (elems,prevEdge) (el : Expr * EdgeOp option) =
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
    path
    |>> List.fold folder ([],None)
    |>> function
        | (lst, None) -> lst
        | (lst, Some e) -> failwith "Will not happen. Final element cannot have outgoing edge."
    |>> List.rev
    |>> Path
    |>> PathExpr
    |>> GExpr

let pGExpr = pPathExpr

// let gExprTerms = chance[pPathExpr; pVar]

// let gExprOpp = new OperatorPrecedenceParser<Expr,unit,unit>()
// let pGExpr = gExprOpp.ExpressionParser
// gExprOpp.TermParser <- gExprTerms <|> brackets pGExpr
// gExprOpp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, fun x y -> (x,Plus,y) |> BinExpr))
// gExprOpp.AddOperator(InfixOperator("-", notFollowedBy (str ">") >>. ws, 1, Associativity.Left, fun x y -> (x,Minus,y))

// let pTTerm = pId |>> TExpr.TTerm |>> TExpr
// let tExprOpp = new OperatorPrecedenceParser<Expr,unit,unit>()
// let pTExpr = tExprOpp.ExpressionParser
// let tExprTerms = chance [pSExpr]
// TODO: Refactor attempt out
// tExprOpp.TermParser <- (tExprTerms) <|> brackets pTExpr
// tExprOpp.AddOperator(PostfixOperator("!", ws, 8, true, fun x -> (x, TPostOp.ALAPApp) |> TPostfixExpr |> TExpr))
// tExprOpp.AddOperator(PostfixOperator("?", ws, 8, true, fun x -> (x, TPostOp.MaybeApp) |> TPostfixExpr |> TExpr))
// tExprOpp.AddOperator(PrefixOperator("$", ws, 9, true, fun x -> (TPreOp.Dollar,x) |> TPrefixExpr |> TExpr))
// tExprOpp.AddOperator(InfixOperator("**", ws, 7, Associativity.Left, fun x y -> (x, MulApp, y) |> TBinExpr |> TExpr)) 
// tExprOpp.AddOperator(InfixOperator("*!*", ws, 7, Associativity.Left, fun x y -> (x, UpToApp, y) |> TBinExpr |> TExpr)) 

// let aExprOpp = new OperatorPrecedenceParser<Expr,unit,unit>()
// let pAExpr = aExprOpp.ExpressionParser
// TODO: Extend for param lists
// let aExprTerms = chance [pTExpr; pGExpr]
// aExprOpp.TermParser <- (attempt aExprTerms) <|> brackets pAExpr
// aExprOpp.AddOperator(InfixOperator("|>", ws, 2, Associativity.Left, fun x y -> (x,Pipe,y) |> ABinExpr |> AExpr))
// aExprOpp.AddOperator(InfixOperator("<|>", ws, 3, Associativity.Left, fun x y -> (x,OrPipe,y) |> ABinExpr |> AExpr))
// TODO: Refactor. This is a code smell, adding operators for second time
// aExprOpp.AddOperator(PrefixOperator("&", ws, 9, true, fun x -> (SPreOp.Amp,x) |> SPrefixExpr |> SExpr))
// aExprOpp.AddOperator(PrefixOperator("not", ws, 1, true, fun x -> (SPreOp.Not,x) |> SPrefixExpr |> SExpr))


// If parser X is defined in terms of parser Y then X must precede Y in this list
// let exprs = [pAExpr; pTExpr; pSExpr; pGExpr]

let exprs = [pSExpr; pVar; pGExpr;]

do pExprNoNCRef := chance exprs
do pExprRef := chance (pNodeCons :: exprs)

let pVExpr =
    pId .>>. opt pDotAccess' |>>
    function
    | v, Some d -> d v
    | v, None -> v |> Var |> VExpr

let pAssignmentExpr =
    let ctor var _ expr  =  (var, expr) |> AssignmentExpr
    pipe3 pVExpr (str "=") pExprOpp ctor

let pExprStatement =
    choice [pAssignmentExpr;]

let pReturn = str "return" >>. pExprOpp |>> Return
let pBecome = str "become" >>. pExprOpp |>> Become
let pTerminatingStatement = (pReturn <|> pBecome) 

// let mExprOpp = new OperatorPrecedenceParser<Expr,unit,unit>()
// let pMExpr = mExprOpp.ExpressionParser
// let mExprTerms = pGExpr
// mExprOpp.TermParser <- mExprTerms <|> brackets pMExpr
// mExprOpp.AddOperator(InfixOperator("and", ws, 2, Associativity.Left, fun x y -> (x,And,y) |> MBinExpr |> MExpr))
// mExprOpp.AddOperator(PrefixOperator("not", ws, 3, true, fun x -> (Not, x) |> MPrefixExpr |> MExpr))

let pMatchCase =
    let cons lhs where _arrow body term = (lhs,where,body,term) |> MatchCase
    let body = many pExprStatement
    let whereClause = (str "where") >>. many pExprOpp
    pipe5 pExprOpp (opt whereClause) (str "->") body pTerminatingStatement cons

let pMatchStatement =
    let cases = (str "|") >>. sepBy1 pMatchCase (str "|")
    pipe3 (str "match") (opt pGExpr) cases (fun _m e c -> (e,c) |> MatchStatement)
    
let pTransformDef =
    let paramCsv = sepBy pId (str ",")
    let paramBrac = brackets paramCsv <|> paramCsv 
    let exprs = many pExprStatement
    let matches = pMatchStatement
    let cons (def, tName, paramLst, eq, exprs, matches) =
        (tName, paramLst, exprs, matches) |> TransformDef
    pipe6 (str "def") pId paramBrac (str "=") exprs matches cons

// Top level parsers
let pStatement =
    choice [pExprStatement |>> ExprStatement; pTypeDef; pTransformDef] |>> Statement

let pProgramUnit = choice [pStatement]

let pProgram = ws >>. choice [pProgramUnit] |> many1 .>> ws

let pFile = pProgram .>> eof

let pBilboFile file encoding =
    runParserOnFile pFile () file encoding

let pBilboStr' str stream =
    runParserOnString pFile () stream str 

let pBilboStr str =
    pBilboStr' str "user input string"