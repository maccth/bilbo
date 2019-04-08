module Bilbo.Parser.Main

open System
open Bilbo.Parser.Parser

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
                pBilboStrPrint codeIn
                codeIn <- ""
                stillReading <- false
            else
                stillReading <- true
    | _ ->
        let file =  argv.[0]
        let result = pBilbo file
        printfn "%A" result
    0