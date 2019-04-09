module Bilbo.SemanticAnalyser.Main

open System
open Bilbo.Parser.Parser
open Bilbo.Parser.Main
open Bilbo.SemanticAnalyser.SemanticAnalyser

[<EntryPoint>]
let main (argv : string []) =
    let repl ast = ast |> bilboStringParser |> bilboSemanticAnalyserPrint
    let normal ast = ast |> bilboStringParser |> bilboSemanticAnalyser
    cli argv "Bilbo semantic analysis REPL" repl normal
    0