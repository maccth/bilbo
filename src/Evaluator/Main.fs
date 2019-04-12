module Bilbo.Evaluator.Main

open Bilbo.Common.Cli
open Bilbo.Parser.Parser
open Bilbo.Evaluator.Evaluator

[<EntryPoint>]
let main argv =
    let repl code = code |> bilboStringParser |> bilboEvaluatorPrint
    let normal code = code |> bilboParser |> bilboEvaluatorPrint
    cli argv "Bilbo evaluator REPL" repl normal
    0
