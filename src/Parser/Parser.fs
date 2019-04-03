module Bilbo.Parser.Parser

open FParsec
open Ast
open Bilbo.Parser
open Bilbo.Parser.Ast

let qp x = printfn "%A" x

// Extensions to FParsec
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
    
// Parser
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

let ws = spaces

let str s = pstring s .>> ws

/// Exact version of str. `stre s` parses the string `s` with no spaces after.
let stre s = pstring s

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

let pVar = pId |>> Var

let pTypeDef =
    let csvIds1 = sepBy1 pId (str ",") 
    let csvIds = sepBy pId (str ",")
    let bracIds = between (str "(") (str ")") csvIds
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

let sExprOpp = new OperatorPrecedenceParser<Expr,unit,unit>()
let sExpr = sExprOpp.ExpressionParser

let pDotAccess =
    let attrs = (str ".") >>. sepBy1 pId (str ".")
    let consDot aLst var = aLst |> List.fold (fun x y ->  (x, y) |> DotAccess |> ObjExpr |> SExpr) (var |> Var) 
    attrs |>> consDot

let pObjInstan =
    let attrs = (sepBy sExpr (str ",")) |> between (str "(") (str ")") 
    let consObj aLst tName = (tName, aLst) |> ObjInstan |> ObjExpr |> SExpr
    attrs |>> consObj

let postIds = choice [pDotAccess; pObjInstan]

let pObjExpr =
    let checkPosts s p =
        match p with
        | Some p' -> p' s
        | None -> Var s
    pipe2 pId (opt postIds) checkPosts
    
let pSExprTerms = choice [pLiteral; pObjExpr; pVar;]

sExprOpp.TermParser <- pSExprTerms <|> between (str "(") (str ")") sExpr

// Vaguely based on Python 3 operator precedence
// https://docs.python.org/3/reference/expressions.html#operator-precedence
let sBinExprOps = [
    "^" , 8, Associativity.Right, SBinOp.Pow;

    "*", 7, Associativity.Right, SBinOp.Times;
    "/", 7, Associativity.Right, SBinOp.Divide;

    "+", 6, Associativity.Right, SBinOp.Plus;
    "-", 6, Associativity.Right, SBinOp.Minus;

    "<", 5, Associativity.Right, SBinOp.LessThan;
    "<=", 5, Associativity.Right, SBinOp.LessThanEq;
    ">", 5, Associativity.Right, SBinOp.GreaterThan;
    ">=", 5, Associativity.Right, SBinOp.GreaterThanEq;
    "==", 5, Associativity.Right, SBinOp.Equal;
    "is", 5, Associativity.Right, SBinOp.Is;
    "!=", 5, Associativity.Right, SBinOp.NotEqual;

    // "not" has precedence 4, but is prefix (unary)
    "and", 3, Associativity.Right, SBinOp.And;
    "or", 2, Associativity.Right, SBinOp.Or;
]

let consSBinExpr op l r = (l,op,r) |> SBinExpr |> SExpr

let addSExprBinOp info =
    let op, prec, assoc, astOp = info
    sExprOpp.AddOperator(InfixOperator(op, ws, prec, assoc, (astOp |> consSBinExpr)))

sExprOpp.AddOperator(PrefixOperator("*", ws, 9, true, fun x -> (SPreOp.Star,x) |> SPrefixExpr |> SExpr))
sExprOpp.AddOperator(PrefixOperator("not", ws, 3, true, fun x -> (SPreOp.Not,x) |> SPrefixExpr |> SExpr))

List.map addSExprBinOp sBinExprOps |> ignore

let pSExpr = sExpr

let pExpr, pExprRef = createParserForwardedToRef()

// TODO:
// Add look-ahead to allow for...
//      let weight = pExpr
// but this will require parsing up to the final `>` and after the initial '<'
// to avoid conflicts with comparison operators
let weight = between (str "(") (str ")") pExpr

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

let pGExprTerms = choice[attempt pPathExpr; pVar]

let gExprOpp = new OperatorPrecedenceParser<Expr,unit,unit>()
let pGExpr = gExprOpp.ExpressionParser
gExprOpp.TermParser <- pGExprTerms <|> between (str "(") (str ")") pGExpr
gExprOpp.AddOperator(InfixOperator("+", ws, 1, Associativity.Right, fun x y -> (x,Plus,y) |> GBinExpr |> GExpr))
gExprOpp.AddOperator(InfixOperator("-", ws, 1, Associativity.Right, fun x y -> (x,Minus,y) |> GBinExpr |> GExpr))

// let aExprOpp = new OperatorPrecedenceParser<Expr,unit,unit>()
// let pAExpr = aExprOpp.ExpressionParser
// aExprOpp.TermParser <- choice [attempt pSExpr; attempt pGExpr] <|> between (str "(") (str ")") pAExpr
// aExprOpp.AddOperator(InfixOperator("|>", ws, 1, Associativity.Left, fun x y -> (x,ABinOp.Pipe,y) |> ABinExpr))
// aExprOpp.AddOperator(InfixOperator("<|>", ws, 2, Associativity.Right, fun x y -> (x,ABinOp.OrPipe,y) |> ABinExpr))
// aExprOpp.AddOperator(PrefixOperator("$", ws, 9, true, fun x -> (APreOp.Dollar,x) |> APrefixExpr))
// aExprOpp.AddOperator(PostfixOperator("!", ws, 9, true, fun x -> (x, APostOp.ALAPApp) |> APostfixExpr))

let exprs = [(*attempt pAExpr;*) pSExpr; pGExpr]

do pExprRef := chance (pNodeCons :: exprs)

do pExprNoNCRef := chance exprs

let pAssignmentExpr =
    let ctor var _ expr  =  (var, expr) |> AssignmentExpr
    pipe3 pId (str "=") pExpr ctor

let pExprStatement =
    choice [pAssignmentExpr;]

let pReturn = str "return" >>. pExpr |>> Return
let pBecome = str "become" >>. pExpr |>> Become
let pTerminatingStatement = (pReturn <|> pBecome) 

let mExprOpp = new OperatorPrecedenceParser<Expr,unit,unit>()
let pMExpr = mExprOpp.ExpressionParser
let pMExprTerms = pGExpr
mExprOpp.TermParser <- pMExprTerms <|> between (str "(") (str ")") pMExpr
mExprOpp.AddOperator(InfixOperator("and", ws, 2, Associativity.Right, fun x y -> (x,And,y) |> MBinExpr |> MExpr))
mExprOpp.AddOperator(PrefixOperator("not", ws, 3, true, fun x -> (Not, x) |> MPrefixExpr |> MExpr))

let pMatchCase =
    let cons lhs where _arrow body term = (lhs,where,body,term) |> MatchCase
    // TODO: Change this to str "->" if possible, will require adding look-ahead in OPP
    let body = many pExprStatement
    let whereClause = (str "where") >>. many pExpr
    pipe5 pMExpr (opt whereClause) (str "=>") body pTerminatingStatement cons

let pMatchStatement =
    let cases = (str "|") >>. sepBy1 pMatchCase (str "|")
    pipe3 (str "match") (opt pGExpr) cases (fun _m e c -> (e,c) |> MatchStatement)
    
let pTransformDef =
    let paramCsv = sepBy pId (str ",")
    let paramBrac = between (str "(") (str ")") paramCsv <|> paramCsv 
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