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

let pLiteral =
    choice [pBoolLit; pStrLit; attempt pIntLit; pFloatLit] |>> LitExpr 

let exprOpp = new OperatorPrecedenceParser<Expr,unit,unit>()
let pExpr = exprOpp.ExpressionParser

let pAssignmentExpr =
    let ctor = fun var _ expr ->  (var, expr) |> AssignmentExpr
    pipe3 pId (str "=") pExpr ctor

let pVar =
    pId |>> Var

let pDotAccess =
    let attrs = (str ".") >>. sepBy1 pId (str ".")
    let consDot aLst var = List.fold (fun x y ->  (x, y) |> DotAccess |> ObjExpr) (var |> Var) aLst 
    attrs |>> consDot

let pObjInstan =
    let attrs = (sepBy1 pExpr (str ",")) |> between (str "(") (str ")") 
    let consObj aLst tName = (tName, aLst) |> ObjInstan |> ObjExpr
    attrs |>> consObj

let posts = choice [pDotAccess; pObjInstan]

let pObjExpr =
    let checkPosts s p =
        match p with
        | Some p' -> p' s
        | None -> Var s
    pipe2 pId (opt posts) checkPosts 

/// Left and bidirectional edge operators `>`, `e>`, `<>` `<e>`
let pLeftEdgeOps =
    tuple3 (str "<") (opt pExpr) (opt (str ">"))
    |>> function
    | l, e, Some r -> e |> Bidir
    | l, e, None -> e |> Left

/// Right edge operators `<`, `<e`
let pRightEdgeOps =
    tuple2 (opt pExpr) (str ">") |>> fst |>> Right

let pEdgeOp = (pLeftEdgeOps <|> pRightEdgeOps) 

// Not correct, will need to be node expr for Bilbo :: operator
let pNode = pVar |>> Node

let pPathExpr =
    let edge = (str ",") >>. (pipe3 pEdgeOp (str ",") pNode (fun e c n -> e,n))
    let edge2 = pEdgeOp .>> followedBy (str "," .>>. pNode)
    let elem = pNode .>>. choice [attempt edge |>> Some; preturn None]
    let path = (sepBy elem (str ",")) |> between (str "[") (str "]")
    // Prepending faster than using @, but :: can't be used as infix function
    let prep e lst = e :: lst
    let folder elems el =
        match el with
        | n, Some (e, n2) ->
            elems
            |> prep n
            |> prep (e|> Edge)
            |> prep n2 
        | n, None  -> prep n elems
    path
    |>> List.fold folder []
    |>> List.rev
    |>> Path
    |>> PathExpr

let pPathExpr2 =
    let edge = pEdgeOp .>> followedBy (str (",") .>>. pNode)
    let commaEdge = str "," >>. edge
    let elem = pNode .>>. opt (pipe2 (followedBy commaEdge) (commaEdge) (fun x y -> y))
    let path = sepBy elem (str ",") |> between (str "[") (str "]")
    let cons e lst = e :: lst
    let folder elems el =
        match el with
        | n, Some e ->
            // Doing this explicitly is faster than doing the fold
            elems |> cons n |> cons (e |> Edge)
        | n, None -> cons n elems
    path
    |>> List.fold folder []
    |>> List.rev
    |>> Path
    |>> PathExpr

let pSimpleExpr =
    choice [pLiteral; pObjExpr; pPathExpr2]

exprOpp.TermParser <- pSimpleExpr <|> between (str "(") (str ")") pExpr

let consBinExpr op l r = (l,op,r) |> BinExpr
exprOpp.AddOperator(InfixOperator("+", ws, 1, Associativity.Right, consBinExpr Plus))
exprOpp.AddOperator(InfixOperator("-", ws, 1, Associativity.Right, consBinExpr Minus))
exprOpp.AddOperator(InfixOperator("*", ws, 2, Associativity.Right, consBinExpr Times))
exprOpp.AddOperator(InfixOperator("/", ws, 2, Associativity.Right, consBinExpr Divide))
exprOpp.AddOperator(InfixOperator("^", ws, 3, Associativity.Right, consBinExpr Pow))
// exprOpp.AddOperator(PrefixOperator("-", ws, 4, true, NegExpr))

   

// // Top level parsers

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