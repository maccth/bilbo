module Bilbo.Evaluator.Main

open Bilbo.Common.Cli
open Bilbo.Common.SymbolTable
open Bilbo.Parser.Parser
open Bilbo.Evaluator.Evaluator

let bilboCli argv =
    let file code = code |> bilboParser |> bilboEvaluator
    let repl code syms = code |> bilboStringParser |> fun ast ->  bilboEvaluatorSymsIn ast syms
    let titleStr = "Bilbo REPL v1.0"
    cli argv titleStr file repl Symbols.Empty

[<EntryPoint>]
let main argv =
    bilboCli argv
    0
