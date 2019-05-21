module Bilbo.Evaluator.Main

open Bilbo.Common.Cli
open Bilbo.Common.SymbolTable
open Bilbo.Parser.Parser
open Bilbo.Evaluator.Evaluator

[<EntryPoint>]
let main argv =
    let file code = code |> bilboParser |> bilboEvaluator
    let repl code syms = code |> bilboStringParser |> fun ast ->  bilboEvaluatorSymsIn ast syms
    cli argv "Bilbo evaluator REPL" file repl Symbols.Empty
    0
