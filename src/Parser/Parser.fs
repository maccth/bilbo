module Bilbo.Parser.Parser

open FParsec
open Ast
open Bilbo.Parser
open FParsec
open FParsec.Primitives
open FParsec
open Bilbo.Parser.Ast
open FParsec
open FParsec

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

let id : Parser<string, unit> =
    let firstChar c = isLetter c || c = '_'
    let middleChar c = firstChar c || isDigit c
    let upToLastChar = many1Satisfy2 firstChar middleChar
    let lastChar = (str "'" <|> str "")
    pnKeyword >>. (pipe2 upToLastChar lastChar (+))

let pTypeDeclaration =
    let csvIds1 = sepBy1 id (str ",")
    let ctor = fun _ name _ attrs -> (name, attrs) |> TypeDeclaration
    pipe4 (str "type") id (str "=") csvIds1 ctor

let pStrLiteral : Parser<Literal, unit> =
    let chars = manySatisfy (fun c -> c <> '"')
    between (stre "\"") (str "\"") chars |>> StringLiteral

let pIntLiteral : Parser<Literal, unit> =
    pint32 .>> notFollowedBy (stre ".") .>> ws |>> int |>> IntLiteral

let pFloatLiteral : Parser<Literal, unit> =
    pfloat .>> ws |>> FloatLiteral

let pBoolLiteral : Parser<Literal, unit> =
    (str "True" <|> str "False")
    |>> function
        | t when t="True" -> true
        | _ -> false
    |>> BoolLiteral

let pLiteral =
    choice [pBoolLiteral; pStrLiteral; attempt pIntLiteral; pFloatLiteral] |>> LiteralExpr 

let exprOpp = new OperatorPrecedenceParser<Expr,unit,unit>()

let pExpr = exprOpp.ExpressionParser

let pAssignmentExpr =
    let ctor = fun var _ expr ->  (var, expr) |> AssignmentExpr
    pipe3 id (str "=") pExpr ctor

let pObjectInstantiation =
    let csvExpr = sepBy1 pExpr (str ",")
    id .>>. (between (str "(") (str ")") csvExpr) |>> ObjectInstantiation |>> ObjectExpr

let pSimpleExpr =
    choice [pLiteral; pObjectInstantiation]

exprOpp.TermParser <- pSimpleExpr

exprOpp.AddOperator(InfixOperator("+", ws, 1, Associativity.Right, fun x y -> (x, Plus, y) |> BinaryExpr))
exprOpp.AddOperator(InfixOperator("-", ws, 1, Associativity.Right, fun x y -> (x, Minus, y) |> BinaryExpr))
exprOpp.AddOperator(InfixOperator("*", ws, 2, Associativity.Right, fun x y -> (x, Times, y) |> BinaryExpr))
exprOpp.AddOperator(InfixOperator("/", ws, 3, Associativity.Right, fun x y -> (x, Divide, y) |> BinaryExpr))

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