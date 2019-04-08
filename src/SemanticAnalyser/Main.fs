module Bilbo.SemanticAnalyser.Main

open System
open Bilbo.Parser.Parser
open Bilbo.SemanticAnalyser.SemanticAnalyser


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
                // TODO: refactor REPLs
                let ast = pBilboStr codeIn
                let ast' = analyseSemantics ast
                printfn "%A" ast'
                codeIn <- ""
                stillReading <- false
            else
                stillReading <- true
    | _ ->
        let file =  argv.[0]
        let ast = pBilbo file
        let ast' = analyseSemantics ast
        printfn "%A" ast'
    0