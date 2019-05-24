module Bilbo.Tests.EvaluatorTests.PrefixExpression

open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

// The not operator is tested in Value.fs

let ampTests = [
    """
    n = 10::20
    a = &n
    b = 10
    ""","Integer node id. Ensure this is returned"

    """
    n = "Hello world"::False
    a = &n
    b = "Hello world"
    ""","String node id. Ensure this is returned"

    """
    type T = p1,p2,p3
    n = T(False,"Hello",34.5)::"Load"
    a = &n
    b = T(False,"Hello",34.5)
    ""","User type object node id. Ensure this is returned"
]

let dblAmpTests = [
    """
    n = 10::20
    a = &&n
    b = 20
    ""","Integer node load. Ensure this is returned"

    """
    n = False::"Hello world"
    a = &&n
    b = "Hello world"
    ""","String node load. Ensure this is returned"

    """
    type T = p1,p2,p3
    n = "NodeId"::T(False,"Hello",34.5)
    a = &&n
    b = T(False,"Hello",34.5)
    ""","User type object node load. Ensure this is returned"
]

[<Tests>]
let test =
    let name = "Ampersand operator tests"
    testList name (ampTests |> abTwinVarTests)

[<Tests>]
let test2 =
    let name = "Double ampersand operator tests"
    testList name (dblAmpTests |> abTwinVarTests)