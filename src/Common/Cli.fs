module Bilbo.Common.Cli

open System
open Argu

// Argu tutorial
// https://fsprojects.github.io/Argu/tutorial.html

type CliArgs =
    | BilboFile of string
    | Debug
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | BilboFile _ -> "specify the Bilbo file"
            | Debug _ -> "specify whether debug information should be printed"

let quote =  "\n\n" + "\"All we have to decide is what to do with the time that is given to us.\" - Gandalf\n"

let cli (cliArgs : string []) (replName : string) fileHandler repl startState =
    let argParser = ArgumentParser.Create<CliArgs>(programName = replName)
    let args = argParser.Parse(inputs=cliArgs, ignoreUnrecognized=true)
    let debugInfo = args.Contains Debug
    match args.Contains BilboFile with
    | true ->
        let file = args.GetResult BilboFile
        let result = file |> fileHandler
        match result with
        | Error e ->
            printfn "%A" e
        | Ok res ->                    
            if debugInfo then
                printfn "%A" res
    | _ ->
        printfn "%s" (replName + quote)
        let mutable codeIn = ""
        let mutable stillReading = false
        let mutable eval = startState
        while true do
            match stillReading with
            | false -> printf "\n... "
            | true -> printf "    "
            let line = Console.ReadLine()
            codeIn <- codeIn + "\n" + line.Replace(";", "")
            if line.EndsWith ";" then
                let evalRes = repl codeIn eval
                match evalRes with
                | Error e ->
                    printfn "%A" e
                | Ok evalOk ->
                    if debugInfo then
                        printfn "%A" evalOk
                    eval <- evalOk
                codeIn <- ""
                stillReading <- false
            else
                stillReading <- true