module Bilbo.Parser.Parser
open FParsec
open Ast

let qp x = printfn "%A" x

let keywords =
    [
        "type"
    ] |> Set.ofList


let isKeyword w = keywords.Contains w

let ws = spaces

let str s = pstring s .>> spaces
/// Exact version of str, no spaces consumed.
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


let csvIds1 =
        sepBy1 id (str ",")

let pTypeDeclaration =
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
    choice [pBoolLiteral; pStrLiteral; attempt pIntLiteral; pFloatLiteral] |>> LiteralExpression

let pExpression =
    choice [pLiteral]

let pAssignmentExpression =
    let ctor = fun var _ expr ->  (var, expr) |> AssignmentExpression
    pipe3 id (str "=") pExpression ctor

// Top level parsers

let pExpressionStatement =
    choice [pAssignmentExpression] |>> ExpressionStatement

let pStatement =
    choice [pExpressionStatement; pTypeDeclaration] |>> Statement

let pProgramUnit = choice [pStatement]

let pProgram = choice [pProgramUnit] |> many1

let pFile = ws >>. pProgram .>> ws .>> eof

let pBilbo file encoding =
    runParserOnFile pFile () file encoding