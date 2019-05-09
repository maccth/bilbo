module Bilbo.Common.Cli

open System

let cli (cliArgs : string []) replName fileHandler repl startState =
    match cliArgs.Length with
    | l when l <> 1 ->
        printfn replName
        let mutable codeIn = ""
        let mutable stillReading = false
        let mutable eval = startState
        while true do
            match stillReading with
            | false -> printf "\n~~> "
            | true -> printf "    "
            let line = Console.ReadLine()
            codeIn <- codeIn + "\n" + line.Replace(";", "")
            if line.EndsWith ";" then
                let evalRes = repl codeIn eval
                match evalRes with
                | Error e ->
                    printfn "%A" e
                | Ok evalOk ->
                    printfn "%A" evalOk
                    eval <- evalOk
                codeIn <- ""
                stillReading <- false
            else
                stillReading <- true
    | _ ->
        let file =  cliArgs.[0]
        let result = file |> fileHandler
        printfn "%A" result