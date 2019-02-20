module Bilbo.Parser.Parser
open FParsec
open Ast

let ws = spaces

let str s = pstring s .>> spaces

let id : Parser<string, unit> =
    let firstChar c = isLetter c || c = '_'
    let middleChar c = firstChar c || isDigit c
    let upToLastChar = many1Satisfy2 firstChar middleChar
    let lastChar = (str "'" <|> str "")
    pipe2 upToLastChar lastChar (+)

let csvIds1 = sepBy1 id (str ",")

let pTypeDeclaration =
    let ctor = (fun _ id _ attrs -> (id, attrs) |> TypeDeclaration)
    pipe4 (str "type") id (str "=") csvIds1 ctor



let pProgram = choice [pTypeDeclaration]

let pFile = ws >>. pProgram .>> ws .>> eof

let pBilbo file encoding =
    runParserOnFile pFile () file encoding