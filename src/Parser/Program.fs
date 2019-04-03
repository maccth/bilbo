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
        let mutable codeIn = ""
        let mutable stillReading = false
        while true do
            match stillReading with
            | false -> printf "\n~> "
            | true -> printf "   "
            let line = Console.ReadLine()
            codeIn <- codeIn + "\n" + line.Replace(";", "")
            if line.EndsWith ";" then
                let result = pBilboStr codeIn
                outputParsedResult result
                codeIn <- ""
                stillReading <- false
            else
                stillReading <- true
    | _ ->
        let file =  argv.[0]
        let result = pBilboFile file System.Text.Encoding.UTF8
        outputParsedResult result
    0