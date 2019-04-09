module Bilbo.SemanticAnalyser.Main

open System
open Bilbo.Parser.Parser
open Bilbo.SemanticAnalyser.SemanticAnalyser


[<EntryPoint>]
let main (argv : string []) =
    match argv.Length with
    | l when l <> 1 ->
        printfn "Starting semantic analysis REPL"
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
                // TODO: Create print alternatives for REPL
                let ast = pBilboStr codeIn
                analyseSemanticsTop ast
                codeIn <- ""
                stillReading <- false
            else
                stillReading <- true
    | _ ->
        let file =  argv.[0]
        let ast = pBilbo file
        analyseSemanticsTop ast
        // analyseSemanticsPrint ast []
    0