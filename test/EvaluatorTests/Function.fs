module Bilbo.Tests.EvaluatorTests.Function

open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

let add1Func = """
    def add1 a = return a+1
    """

let negFunc = """
    def neg a = return -1*a
    """

let sumFunc = """
    def sum(x,y) = return x+y
    """

let avg4Func = """
    def avg(a,b,c,d) =
        return (a+b+c+d)/4
    """

let singleParamFunctionApplication = [
    add1Func + """
    a = 10 >> add1
    """, 11 |> Int |> Value, "monic function, 1 stage pipeline";

    add1Func + """
    a = 10 >> add1 |> add1
    """, 12 |> Int |> Value, "monic func, 2 stage pipeline";

    add1Func + """
    a = 10 >> add1 |> add1 |> add1
    """, 13 |> Int |> Value, "monic func, 3 stage pipeline";

    add1Func + negFunc + """
    a = 100 >> add1 |> neg |> add1
    """, -100 |> Int |> Value, "2 monic funcs, 3 stage pipeline"

    add1Func + """
    add3 = add1 |> add1 |> add1
    a = 10 >> add3
    """, 13 |> Int |> Value, "enpipe input into monic func pipeline bound to identifier"

    add1Func + """
    add3 = add1 |> add1 |> add1
    a = 10 >> add3 |> add1
    """, 14 |> Int |> Value, "enpipe input into monic func pipeline composed with monic func"
]

let partialFunctionApplication = [
    sumFunc + """
    x = 10 >> sum
    a = 20 >> x
    """, 30 |> Int |> Value, "2 arg function applied partially, one arg at a time"

    avg4Func + """
    c = 10 >> avg
    d = 20 >> c
    e = 30 >> d
    a = 40 >> e
    """, 25 |> Int |> Value, "4 arg function applied partially, one arg at a time"
]

let paramListAppliction = [
    sumFunc + """
    a = (-345,72) >> sum
    """, -345+72 |> Int |> Value, "2 arg param list enpiped into single function"

    sumFunc + add1Func + """
    a = (56127,-864234) >> sum |> add1 |> add1
    """, (56127-864234+1+1) |> Int |> Value, "2 arg param list enpiped into 3 stage pipeline"

    sumFunc + """
    a = 10 >> sum |> sum
    a = 20 >> a
    a = -5 >> a
    """, 25 |> Int |> Value, "3 stage partial pipeline application"
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

[<Tests>]
let tests2 =
    let name = "basic function application tests"
    testList name (partialFunctionApplication |> quickFunctionAppTests |> singleVarTests)

[<Tests>]
let tests3 =
    let name = "basic function application tests"
    testList name (paramListAppliction |> quickFunctionAppTests |> singleVarTests)