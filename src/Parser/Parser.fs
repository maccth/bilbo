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

let str s = pstring s .>> spaces
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
    let csvIds = sepBy1 pId (str ",") 
    let bracIds = between (str "(") (str ")") csvIds
    let ctor = fun _ name _ attrs -> (name, attrs) |> TypeDeclaration
    pipe4 (str "type") pId (str "=") (bracIds <|> csvIds) ctor

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

let pLitExpr =
    choice [pBoolLit; pStrLit; attempt pIntLit; pFloatLit] |>> LitExpr 

let sExprOpp = new OperatorPrecedenceParser<SExpr,unit,unit>()
let pSExpr = sExprOpp.ExpressionParser

let pDotAccess =
    let attrs = (str ".") >>. sepBy1 pId (str ".")
    let consDot aLst var = List.fold (fun x y ->  (x, y) |> DotAccess |> ObjExpr) (var |> SExpr.Var) aLst 
    attrs |>> consDot

let pObjInstan =
    let attrs = (sepBy1 pSExpr (str ",")) |> between (str "(") (str ")") 
    let consObj aLst tName = (tName, aLst) |> ObjInstan |> ObjExpr
    attrs |>> consObj

let posts = choice [pDotAccess; pObjInstan]

let pObjExpr =
    let checkPosts s p =
        match p with
        | Some p' -> p' s
        | None -> SExpr.Var s
    pipe2 pId (opt posts) checkPosts

let pSimpleExpr = choice [pLitExpr; pObjExpr;]

sExprOpp.TermParser <- pSimpleExpr <|> between (str "(") (str ")") pSExpr

let consBinExpr op l r = (l,op,r) |> BinExpr
sExprOpp.AddOperator(InfixOperator("+", ws, 1, Associativity.Right, consBinExpr Plus))
sExprOpp.AddOperator(InfixOperator("-", ws, 1, Associativity.Right, consBinExpr Minus))
sExprOpp.AddOperator(InfixOperator("*", ws, 2, Associativity.Right, consBinExpr Times))
sExprOpp.AddOperator(InfixOperator("/", ws, 2, Associativity.Right, consBinExpr Divide))
sExprOpp.AddOperator(InfixOperator("^", ws, 3, Associativity.Right, consBinExpr Pow))

let pNodeCons =
    pipe3 sExprOpp.TermParser (str "::") sExprOpp.TermParser (fun i _ l -> (i,l))

/// Left and bidirectional edge operators `<`, `<e`, `<>` `<e>`
let pRightEdgeOps =
    tuple3 (str "<") (opt pSExpr) (opt (str ">"))
    |>> function
    | l, e, Some r -> e |> Bidir
    | l, e, None -> e |> Left

/// Right edge operators `>`, `e>`
let pLeftEdgeOps =
    tuple2 (opt pSExpr) (str ">") |>> fst |>> Right

let pEdgeOp = (pLeftEdgeOps <|> pRightEdgeOps) 

// // Not correct, will need to be node expr for Bilbo :: operator
let pNodeExpr = choice [attempt pNodeCons |>> NodeCons; pId |>> Var]

let pPathExpr =
    let edge = pEdgeOp .>> followedBy (str (",") .>>. pNodeExpr)
    let commaEdge = str "," >>. edge
    let elem = pNodeExpr .>>. opt (pipe2 (followedBy commaEdge) (commaEdge) (fun x y -> y))
    let path = sepBy elem (str ",") |> between (str "[") (str "]")
    let cons e lst = e :: lst
    let folder elems el =
        match el with
        | n, Some e ->
            // Doing this explicitly is faster than doing the fold
            elems |> cons (n |> Node) |> cons (e |> Edge)
        | n, None -> cons (n |> Node) elems
    path
    |>> List.fold folder []
    |>> List.rev
    |>> Path
    |>> PathExpr

// TODO: Join this and the above together, by parsing the known correct lists
// let pPathExpr =
//     let edge = pipe3 pEdgeOp (str ",") pNodeExpr (fun e _ n -> e,n)
//     let commaEdge = str "," >>. edge
//     let elem = pNodeExpr .>>. opt (pipe2 (followedBy commaEdge) (commaEdge) (fun x y -> y))
//     let path = sepBy elem (str ",") |> between (str "[") (str "]")
//     let cons e lst = e :: lst
//     let folder elems el =
//         match el with
//         | n, Some(e, n2) ->
//             // Doing this explicitly is faster than doing the fold
//             ((n,e,n2) |> Edge) :: elems 
//         | n, None -> (Node n) :: elems
//     path
//     |>> List.fold folder []
//     |>> List.rev
//     |>> Path
//     |>> PathExpr

let pExpr =
    choice [
        attempt pNodeCons |>> SExpr.NodeCons |>> SExpr;
        pSExpr |>> SExpr;
        pPathExpr |>> GExpr;
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

let pBilboStrE str eStream =
    runParserOnString pProgram () eStream str 

let pBilboStr str =
    pBilboStrE str ""