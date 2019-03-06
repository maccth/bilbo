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

let pVarId : Parser<string, unit> =
    let firstChar c = isLetter c || c = '_'
    let middleChar c = firstChar c || isDigit c
    let upToLastChar = many1Satisfy2 firstChar middleChar
    let lastChar = (str "'" <|> str "")
    pnKeyword >>. (pipe2 upToLastChar lastChar (+))

let pTypeDeclaration =
    let csvIds1 = sepBy1 pVarId (str ",")
    let ctor = fun _ name _ attrs -> (name, attrs) |> TypeDeclaration
    pipe4 (str "type") pVarId (str "=") csvIds1 ctor

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
    pipe3 pVarId (str "=") pExpr ctor

let pObjectInstantiation =
    let csvExpr = sepBy1 pExpr (str ",")
    pVarId .>>. (between (str "(") (str ")") csvExpr) |>> ObjInstantiation |>> ObjExpr

let pSimpleExpr =
    choice [pLiteral; pObjectInstantiation]

exprOpp.TermParser <- pSimpleExpr

let consBinExpr op l r = (l,op,r) |> BinExpr
exprOpp.AddOperator(InfixOperator("+", ws, 1, Associativity.Right, consBinExpr Plus))
exprOpp.AddOperator(InfixOperator("-", ws, 1, Associativity.Right, consBinExpr Minus))
exprOpp.AddOperator(InfixOperator("*", ws, 2, Associativity.Right, consBinExpr Times))
exprOpp.AddOperator(InfixOperator("/", ws, 2, Associativity.Right, consBinExpr Divide))

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