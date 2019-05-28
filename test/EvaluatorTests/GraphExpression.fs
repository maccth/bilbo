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

let pathComprehensions = [
    """
    a = [&=: ]
    b = []
    ""","Ensure id setting works (does nothing) with an empty graph."

    """
    a = [&=: "a"]
    b = ["a"::"a"]
    ""","Ensure id setting works with a single node."

    """
    a = [&=: "a", "b"]
    b = ["a"::"a", "b"::"b"]
    ""","Ensure id setting works with a pair of nodes."

    """
    na = "a"::10
    a = [&=: na, "b"]
    b = ["a"::10, "b"::"b"]
    """, "Ensure id setting works with nodes in the expression by selecting id and load of node present."

    """
    a = [&=: 1,2,3,4,5,>,6]
    b = [1::1,2::2,3::3,4::4,5::5,>,6::6]
    """, "Ensure id setting works with edges present in the path expression."

    """
    a = [#=10: ]
    b = []
    ""","Ensure load setting works (does nothing) with an empty graph."

    """
    a = [#=False: "a"]
    b = ["a"::False]
    ""","Ensure load setting works with a single node."

    """
    a = [#=0: "a", "b"]
    b = ["a"::0, "b"::0]
    ""","Ensure load setting works with a pair of nodes."

    """
    na = "a"::10
    a = [#=300: na, "b"]
    b = ["a"::300, "b"::300]
    """, "Ensure load setting works with nodes in the expression by selecting id of node present and set load."

    """
    a = [#=0: 1,2,3,4,5,>,6]
    b = [1::0,2::0,3::0,4::0,5::0,>,6::0]
    """, "Ensure load setting works with edges present in the path expression."

    nodes + """
    a = [<10>: ]
    b = []
    ""","Ensure edge setting does nothing in an empty graph."

    nodes + """
    a = [10>: na]
    b = [na]
    ""","Ensure edge setting does nothing in a graph with a single node."

    nodes + """
    a = [70>: na,na]
    b = [na,70>,na]
    ""","Ensure edge setting creates a loop in a graph with a single node repeated."

    nodes + """
    a = [<45+5>: na,nb,nc,nc,nd,na]
    b = [na,<50>,nb,<50>,nc,<50>,nc,<50>,nd,<50>,na]
    ""","Ensure edge setting works for longer path expressions."
]

[<Tests>]
let test =
    let name = "Standard path expressions"
    testList name (standardPathExpressions |> abTwinVarTests)


[<Tests>]
let test2 =
    let name = "Nested path expressions"
    testList name (nestedPathExpressions |> abTwinVarTests)

[<Tests>]
let test3 =
    let name = "Path comprehensions"
    testList name (pathComprehensions |> abTwinVarTests)