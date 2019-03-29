module Bilbo.Parser.Parser

open FParsec
open Ast
open Bilbo.Parser
open Bilbo.Parser.Ast

let qp x = printfn "%A" x

let keywords =
    [
        "type"
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
    pnKeyword >>. (pipe2 upToLastChar lastChar (+))

let pTypeDeclaration =
    let csvIds1 = sepBy1 pId (str ",") 
    let csvIds = sepBy pId (str ",")
    let bracIds = between (str "(") (str ")") csvIds
    let ctor = fun _ name _ attrs -> (name, attrs) |> TypeDeclaration
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

let pLit =
    choice [pBoolLit; pStrLit; attempt pIntLit; pFloatLit] |>> Literal 

let sExprOpp = new OperatorPrecedenceParser<SExpr,unit,unit>()
let sExpr = sExprOpp.ExpressionParser

let pDotAccess =
    let attrs = (str ".") >>. sepBy1 pId (str ".")
    let consDot aLst var = List.fold (fun x y ->  (x, y) |> DotAccess |> ObjExpr) (var |> SVar) aLst 
    attrs |>> consDot

let pObjInstan =
    let attrs = (sepBy sExpr (str ",")) |> between (str "(") (str ")") 
    let consObj aLst tName = (tName, aLst) |> ObjInstan |> ObjExpr
    attrs |>> consObj

let posts = choice [pDotAccess; pObjInstan]

let pObjExpr =
    let checkPosts s p =
        match p with
        | Some p' -> p' s
        | None -> SVar s
    pipe2 pId (opt posts) checkPosts
    
let pSimpleExpr = choice [pLit; pObjExpr;]

sExprOpp.TermParser <- pSimpleExpr <|> between (str "(") (str ")") sExpr

let sExprOps = [
    "+", 1, Associativity.Right, SBinOp.Plus;
    "-", 1, Associativity.Right, SBinOp.Minus;
    "*", 2, Associativity.Right, SBinOp.Times;
    "/", 2, Associativity.Right, SBinOp.Divide;
    "^", 3, Associativity.Right, SBinOp.Pow;
]

let consBinExpr op l r = (l,op,r) |> SExpr.BinExpr

let addSExprOp info =
    let op, prec, assoc, astOp = info
    sExprOpp.AddOperator(InfixOperator(op, ws, prec, assoc, consBinExpr astOp))

List.map addSExprOp sExprOps |> ignore

let pNodeCons =
    pipe3 sExprOpp.TermParser (str "::") sExprOpp.TermParser (fun i _ l -> (i,l))
 
let sExprs = choice [ attempt pNodeCons |>> SExpr.NodeCons; sExpr]
let pSExpr = pipe2 (opt (str "*")) sExprs <| fun star expr ->
    match star with
    | Some _ -> NodeId expr
    | None -> expr

/// Left and bidirectional edge operators `<`, `<e`, `<>` `<e>`
let pRightEdgeOps =
    tuple3 (str "<") (opt sExpr) (opt (str ">"))
    |>> function
    | l, e, Some r -> e |> Bidir
    | l, e, None -> e |> Left

/// Right edge operators `>`, `e>`
let pLeftEdgeOps =
    tuple2 (opt sExpr) (str ">") |>> fst |>> Right

let pEdgeOp = (pLeftEdgeOps <|> pRightEdgeOps)

let pNodeExpr = choice [attempt pNodeCons |>> NodeCons; pId |>> NVar]

let pPathExpr =
    let edge = pEdgeOp .>> followedBy (str (",") .>>. pNodeExpr)
    let commaEdge = str "," >>. edge
    let elem = pNodeExpr .>>. opt (pipe2 (followedBy commaEdge) (commaEdge) (fun x y -> y))
    let path = sepBy elem (str ",") |> between (str "[") (str "]")
    let folder (elems,prevEdge) (el : NodeExpr * EdgeOp option) =
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

let pGVar = pId |>> GVar

let pGraphExprs = choice[pGVar; pPathExpr]

let gExprOpp = new OperatorPrecedenceParser<GExpr,unit,unit>()
let pGExpr = gExprOpp.ExpressionParser
gExprOpp.TermParser <- pGraphExprs <|> between (str "(") (str ")") pGExpr
gExprOpp.AddOperator(InfixOperator("+", ws, 1, Associativity.Right, fun x y -> (x,Plus,y) |> BinExpr))
gExprOpp.AddOperator(InfixOperator("-", ws, 1, Associativity.Right, fun x y -> (x,Minus,y) |> BinExpr))

let pExpr =
    choice [
        pSExpr |>> SExpr;
        pGExpr |>> GExpr;
    ]

let pAssignmentExpr =
    let ctor = fun var _ expr ->  (var, expr) |> AssignmentExpr
    pipe3 pId (str "=") pExpr ctor

// Top level parsers
let pExprStatement =
    choice [pAssignmentExpr] |>> ExprStatement

let pStatement =
    choice [pExprStatement; pTypeDeclaration] |>> Statement

let pProgramUnit = choice [pStatement]

let pProgram = ws >>. choice [pProgramUnit] |> many1 .>> ws

let pFile = pProgram .>> eof

let pBilboFile file encoding =
    runParserOnFile pFile () file encoding

let pBilboStr' str stream =
    runParserOnString pProgram () stream str 

let pBilboStr str =
    pBilboStr' str "user input string"