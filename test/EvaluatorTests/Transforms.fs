module Bilbo.Tests.EvaluatorTests.Transforms

open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

let transformTests = [
    """
    def node(g) =
        match g
        | [x] -> return x
    a = "aNode"::23984
    b = [a] >> node
    """, "Match on single node, return it";

    """
    def number(g) =
        match g
        | [x] -> return 2.7182818
    a = 2.7182818
    b = ["node"::"load"] >> number
    """, "Match on single node, return another value";

    """
    a = 2.7182818
    def number(g) =
        match g
        | [x] -> return a
    b = ["node"::"load"] >> number
    """, "Match on single node, return value from function closure";
]

let quickTwinVarTest codeStr des =
    (codeStr, "a", "b", des) 

let quickTwinVarTests tLst =
    tLst
    |> List.map (fun (c,d) -> quickTwinVarTest c d)
    |> twinVarTests

[<Tests>]
let test =
    let name = "basic transform tests"
    testList name (transformTests |> quickTwinVarTests)