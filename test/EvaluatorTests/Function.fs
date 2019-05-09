module Bilbo.Tests.EvaluatorTests.Function

open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

let singleParamFunctionApplication = [
    """
    def add1 x = return x+1
    a = 10 >> add1
    """, 11 |> Int |> Value, "Basic function application";
]


let quickFunctionAppTest codeStr mean des =
    let var = "a"
    codeStr, var, mean, des

let quickFunctionAppTests testDatalst =
    List.map (fun (s,m,d) -> quickFunctionAppTest s m d) testDatalst

[<Tests>]
let tests =
    let name = "basic function application tests"
    testList name (singleParamFunctionApplication |> quickFunctionAppTests |> singleVarTests)
