module Bilbo.Parser.Main
open System
open FParsec
open Ast
open Parser

let outputParsedResult res =
    match res with
    | Success(res', _s, _p) ->
        printf "The AST of the input is:\n%A\n" res'
    | Failure(msg, err, u) ->
        printfn "%s" msg
        printfn "%A" err 
        printfn "%A" u

[<EntryPoint>]
let main (argv : string []) =
    match argv.Length with
    | l when l <> 1 ->
        printfn "Starting parsing REPL"
        while true do
            printf "\n~> "
            let line = Console.ReadLine()
            let result = pBilboStr line
            outputParsedResult result
        0
    | _ ->
        let file =  argv.[0]
        let result = pBilboFile file System.Text.Encoding.UTF8
        outputParsedResult result
        0