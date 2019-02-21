module Bilbo.Parser.Main
open System
open FParsec

open Ast
open Parser

[<EntryPoint>]
let main (argv : string []) =
    match argv.Length with
    | l when l <> 1 ->
        printfn "Pass in a file as the first and only argument"
        exit 1
    | _ ->
        let file =  argv.[0]
        let result = pBilbo file System.Text.Encoding.UTF8
        match result with
        | Success(res, _s, _p) ->
            printf "The AST of the input file is:\n%A\n" res
        | Failure(msg, err, u) ->
            printfn "%s" msg
            printfn "%A" err 
            printfn "%A" u
        0