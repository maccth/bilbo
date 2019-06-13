module Bilbo.Tests.EvaluatorTests.GraphExpression

open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

let standardPathExpressions = [
    nodes + """
    got = [na,na,na,na]
    exp = [na]
    ""","Ensure that identical nodes collapse into a single node."

    nodes + """
    got = [na,nb,na,nb]
    exp = [nb,na]
    """, "Ensure that multiple identical nodes collapse each into one single node."

    nodes + """
    got = [na,>,nb]
    exp = [nb,<,na]
    """, "Ensure that left and right edge operators are opposites."

    nodes + """
    got = [na,>,nb,>,na]
    exp = [nb,<>,na]
    """, "Ensure that a left and right edge are equivalent to a double edge operator."

    nodes + """
    got = [nb,<>,na]
    exp = [na,>,nb,>,na]
    """, "Ensure that a double edge operator is equiavelnt to a left and right edge."

    """
    na1 = "a"::100
    na2 = "a"::200
    got = [na1,na2] 
    exp = [na2]
    """, "Ensure that id clashes favour the right-most node."

    """
    na1 = "a"::100
    na2 = "a"::200
    got = [na1,na2,na1] 
    exp = [na1]
    """, "Ensure that multiple id clashes favour the right-most node."
]

let nestedPathExpressions = [
    nodes + """
    got = [na,>,[nb]]
    exp = [na,>,nb]
    """, "Ensure an edge to a single nested node constructs only one edge."

    nodes + """
    got = [[na],>,[nb]]
    exp = [na,>,nb]
    """, "Ensure an edge between single nested nodes constructs only one edge."

    nodes + """
    got = [[na],<>,[nb]]
    exp = [na,<>,nb]
    """, "Ensure the double edge operator between single nested nodes constructs only two edges."

    nodes + """
    got = [[na],>,[nb,>,nc]]
    exp = [na,>,nb,>,nc] + [na,>,nc]
    """, "Ensure that edges witin nested path expressions are constructed."

    nodes + """
    got = [[na],>,[nb,nc],>,[nd,ne]]
    exp = [na,>,nb,>,nd] + [nb,>,ne] + [na,>,nc,>,nd] + [nc,>,ne]
    """, "Ensure that more than two nested paths work."

    nodes + """
    g = [na,>,nb,>,nc]
    got = [nd,>,g]
    exp = [nd,>,na,>,nb,>,nc] + [nd,>,nb] + [nd,>,nc]
    ""","Ensure that graphs constructed outside of path expressions can still be used as nested path expressions."

    nodes + """
    got = [na,>,[nb,>,[nc,nd]]]
    exp = [na,>,nd,<,nb,>,nc] + [na,>,nb] + [na,>,nc]
    """, "Ensure that the nesting of nested path expressions correctly constructs the graph."
]

let pathComprehensions = [
    """
    got = [&=: ]
    exp = []
    ""","Ensure id setting works (does nothing) with an empty graph."

    """
    got = [&=: "a"]
    exp = ["a"::"a"]
    ""","Ensure id setting works with a single node."

    """
    got = [&=: "a", "b"]
    exp = ["a"::"a", "b"::"b"]
    ""","Ensure id setting works with a pair of nodes."

    """
    na = "a"::10
    got = [&=: na, "b"]
    exp = ["a"::10, "b"::"b"]
    """, "Ensure id setting works with nodes in the expression by selecting id and load of node present."

    """
    got = [&=: 1,2,3,4,5,>,6]
    exp = [1::1,2::2,3::3,4::4,5::5,>,6::6]
    """, "Ensure id setting works with edges present in the path expression."

    """
    got = [#=10: ]
    exp = []
    ""","Ensure load setting works (does nothing) with an empty graph."

    """
    got = [#=False: "a"]
    exp = ["a"::False]
    ""","Ensure load setting works with a single node."

    """
    got = [#=0: "a", "b"]
    exp = ["a"::0, "b"::0]
    ""","Ensure load setting works with a pair of nodes."

    """
    na = "a"::10
    got = [#=300: na, "b"]
    exp = ["a"::300, "b"::300]
    """, "Ensure load setting works with nodes in the expression by selecting id of node present and set load."

    """
    got = [#=0: 1,2,3,4,5,>,6]
    exp = [1::0,2::0,3::0,4::0,5::0,>,6::0]
    """, "Ensure load setting works with edges present in the path expression."

    nodes + """
    got = [<10>: ]
    exp = []
    ""","Ensure edge setting does nothing in an empty graph."

    nodes + """
    got = [10>: na]
    exp = [na]
    ""","Ensure edge setting does nothing in a graph with a single node."

    nodes + """
    got = [70>: na,na]
    exp = [na,70>,na]
    ""","Ensure edge setting creates a loop in a graph with a single node repeated."

    nodes + """
    got = [<45+5>: na,nb,nc,nc,nd,na]
    exp = [na,<50>,nb,<50>,nc,<50>,nc,<50>,nd,<50>,na]
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