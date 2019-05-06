module Bilbo.Tests.EvaluatorTests.Expression

open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

let singleVarAssignTests = [
    "a = 10", "a", 10 |> Int |> Value, "a simple test";
]

[<Tests>]
let tests =
    testList "basic tests" (singleVarTests singleVarAssignTests)