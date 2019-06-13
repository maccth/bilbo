module Bilbo.Tests.EvaluatorTests.PrefixExpression

open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

// The not operator is tested in Value.fs

let ampTests = [
    """
    n = 10::20
    got = &n
    exp = 10
    ""","Integer node id. Ensure this is returned"

    """
    n = "Hello world"::False
    got = &n
    exp = "Hello world"
    ""","String node id. Ensure this is returned"

    """
    type T = p1,p2,p3
    n = T(False,"Hello",34.5)::"Load"
    got = &n
    exp = T(False,"Hello",34.5)
    ""","User type object node id. Ensure this is returned"
]

let hashTests = [
    """
    n = 10::20
    got = #n
    exp = 20
    ""","Integer node load. Ensure this is returned"

    """
    n = False::"Hello world"
    got = #n
    exp = "Hello world"
    ""","String node load. Ensure this is returned"

    """
    type T = p1,p2,p3
    n = "NodeId"::T(False,"Hello",34.5)
    got = #n
    exp = T(False,"Hello",34.5)
    ""","User type object node load. Ensure this is returned"
]

[<Tests>]
let test =
    let name = "Ampersand operator tests"
    testList name (ampTests |> gotExpTwinVarTests)

[<Tests>]
let test2 =
    let name = "hash operator tests"
    testList name (hashTests |> gotExpTwinVarTests)