module Bilbo.Tests.EvaluatorTests.GraphExpression

open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

let standardPathExpressions = [
    nodes + """
    a = [na,na,na,na]
    b = [na]
    ""","Ensure that identical nodes collapse into a single node."

    nodes + """
    a = [na,nb,na,nb]
    b = [nb,na]
    """, "Ensure that multiple identical nodes collapse each into one single node."

    nodes + """
    a = [na,>,nb]
    b = [nb,<,na]
    """, "Ensure that left and right edge operators are opposites."

    nodes + """
    a = [na,>,nb,>,na]
    b = [nb,<>,na]
    """, "Ensure that a left and right edge are equivalent to a double edge operator."

    nodes + """
    a = [nb,<>,na]
    b = [na,>,nb,>,na]
    """, "Ensure that a double edge operator is equiavelnt to a left and right edge."

    """
    na1 = "a"::100
    na2 = "a"::200
    a = [na1,na2] 
    b = [na2]
    """, "Ensure that id clashes favour the right-most node."

    """
    na1 = "a"::100
    na2 = "a"::200
    a = [na1,na2,na1] 
    b = [na1]
    """, "Ensure that multiple id clashes favour the right-most node."
]

let nestedPathExpressions = [
    nodes + """
    a = [na,>,[nb]]
    b = [na,>,nb]
    """, "Ensure an edge to a single nested node constructs only one edge."

    nodes + """
    a = [[na],>,[nb]]
    b = [na,>,nb]
    """, "Ensure an edge between single nested nodes constructs only one edge."

    nodes + """
    a = [[na],<>,[nb]]
    b = [na,<>,nb]
    """, "Ensure the double edge operator between single nested nodes constructs only two edges."

    nodes + """
    a = [[na],>,[nb,>,nc]]
    b = [na,>,nb,>,nc] + [na,>,nc]
    """, "Ensure that edges witin nested path expressions are constructed."

    nodes + """
    a = [[na],>,[nb,nc],>,[nd,ne]]
    b = [na,>,nb,>,nd] + [nb,>,ne] + [na,>,nc,>,nd] + [nc,>,ne]
    """, "Ensure that more than two nested paths work."

    nodes + """
    g = [na,>,nb,>,nc]
    a = [nd,>,g]
    b = [nd,>,na,>,nb,>,nc] + [nd,>,nb] + [nd,>,nc]
    ""","Ensure that graphs constructed outside of path expressions can still be used as nested path expressions."

    nodes + """
    a = [na,>,[nb,>,[nc,nd]]]
    b = [na,>,nd,<,nb,>,nc] + [na,>,nb] + [na,>,nc]
    """, "Ensure that the nesting of nested path expressions correctly constructs the graph."
]

[<Tests>]
let test =
    let name = "Standard path expressions"
    testList name (standardPathExpressions |> abTwinVarTests)


[<Tests>]
let test2 =
    let name = "Nested path expressions"
    testList name (nestedPathExpressions |> abTwinVarTests)
