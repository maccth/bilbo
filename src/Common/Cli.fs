module Bilbo.Common.Cli

open System

let cli (cliArgs : string []) replName repl fileHandler =
    match cliArgs.Length with
    | l when l <> 1 ->
        printfn replName
        let mutable codeIn = ""
        let mutable stillReading = false
        while true do
            match stillReading with
            | false -> printf "\n~> "
            | true -> printf "   "
            let line = Console.ReadLine()
            codeIn <- codeIn + "\n" + line.Replace(";", "")
            if line.EndsWith ";" then
                codeIn |> repl
                codeIn <- ""
                stillReading <- false
            else
                stillReading <- true
    | _ ->
        let file =  cliArgs.[0]
        let result = file |> fileHandler
        printfn "%A" result