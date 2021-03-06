module Bilbo.Tests.EvaluatorTests.Transform

open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

let multipleMatchCasesTransformTests = [
    nodes + """
    def graph(g) =
        match g
        | [a] => return g
        | [] =>
            n = "startNode"::200
            return [n]
    got = [na] >> graph
    exp = [na]        
    ""","Match first case, doesn't go to second."

    nodes + """
    def graph(g) =
        match g
        | [a] => return g
        | [] =>
            n = "startNode"::200
            return [n]
    got = [] >> graph
    exp = ["startNode"::200]        
    ""","Fail first case, go to second."

    nodes + """
    def graph(g) =
        match g
        | [a] => return []
        | [a] => return g
    got = [na] >> graph
    exp = []        
    ""","Both cases the same. Ensure first is taken."

    nodes + """
    def graph(g) =
        match g
        | [a] where a == nb => return []
        | [a] => return g
    got = [na] >> graph
    exp = [na]        
    ""","Both pattern graphs the same, but first where clause fails. Ensure second is taken."

    nodes + """
    def graph(g) =
        match g
        | [a] where a == na => return []
        | [a] => return g
    got = [na] >> graph
    exp = []        
    ""","Both pattern graphs the same, but first where clause passes. Ensure first is taken."

    """
    def change(g) = match g | [x,>,y] =>
        x = x::20
        become [x,>,y]
    na = "a"::0
    nb = "b"::1
    nc = "c"::2
    g = [na,>,nb] + [na,>,nc]
    got = g >> change
    exp = ["a"::20,>,nb] + ["a"::20,>,nc]
    """, "Changing a node in the graph. Ensure that edge deletion/preservation on works correctly when nodes are changed"

    """
    def change(g) = match g | [x,>,y] =>
        x = x::20
        become [x,30>,y]
    na = "a"::0
    nb = "b"::1
    nc = "c"::2
    g = [na,>,nb] + [na,20>,nc]
    got = g >> change
    exp = ["a"::20, 30>, nb] + ["a"::20, 20>, nc]
    """, "Changing a node in the graph. Ensure that edge deletion/preservation on works correctly when nodes and edges are changed"
]

let collectionTests = [
    nodes + """
    def delNode(g) = match g | [x] => become []
    g = [na,nb]
    got = g >> delNode
    exp = [na] |&| [nb]
    """, "Deleting a node resulting in a collection of two graphs. Ensure both graphs are returned in a collection."

    nodes + """
    def delNode(g) = match g | [x] => become []
    g = [na,nb]
    got = g >> delNode |> delNode
    exp = []
    """, "Deleting a node twice. Returns a collection of two empty graphs. Ensure that these are collapsed to one graph."

    nodes + """
    def delNode(g) = match g | [x] => become []
    g = [na,>,nb,>,nc]
    got = g >> delNode
    exp =
        [nb,>,nc]
        |&| [na,nc]
        |&| [na,>,nb]
    """, "Deleting a node from a graph with multiple nodes. Ensure that edge deletion/preservation works correctly on collections"
]

let patternGraphBinOps = [

    nodes + """
    def middle(g) =
        match g
        | [a,>,b] and [b,>,c] => return [b]
    g = [na,>,nb,>,nc]
    got = g >> middle
    exp = [nb]
    """, "And condition used not in a negative application condition"

    nodes + """
    def endPair(g) =
        match g
        | [a,>,b] and not [b,>,c] => return [a,>,b]
    g = [na,>,nb,>,nc]
    got = g >> endPair
    exp = [nb,>,nc]
    """, "Negative application conditions ensure only one subgraph matches"

    nodes + """
    def endPair(g) =
        match g
        | [a,>,b] and not [b,>,c] => return [a,>,b]
    g = [na,>,nb,>,nc] + [nd,>,ne,>,nc]
    got = g >> endPair
    exp = [nb,>,nc] |&| [ne,>,nc]
    """, "Negative application conditions ensure a collection of only two graphs is returned"
]

let patternGraphOrOps = [
    nodes + """
    def edge(g) = match g
        | [a,<>,b]
        or [a,>,b] => return [a,b]
    g = [na,<>,nb]
    got = g >> edge
    exp = [na,nb]
    """, "Or application conditions in positive graph"

    nodes + """
    def deadLink(g) =
        match g
        | [a,>,b] and not ([b,>,c] or [d,>,b]) => return [a,>,b]
    g = [na,>,nb] + [nc,>,na,<,nd] + [nc,>,nd,>,ne]
    got = g >> deadLink
    exp = [na,>,nb] |&| [nd,>,ne]
    """, "Or application conditions in negative graph"
]


[<Tests>]
let test5 =
    let name = "Multiple match cases tests"
    testList name (multipleMatchCasesTransformTests |> gotExpTwinVarTests)

[<Tests>]
let test6 =
    let name = "Tests involving collections due to multiple subgraph matches"
    testList name (collectionTests |> gotExpTwinVarTests)

[<Tests>]
let test7 =
    let name = "Tests involving pattern graph binary operations"
    testList name (patternGraphBinOps |> gotExpTwinVarTests)

[<Tests>]
let test8 =
    let name = "Tests involving pattern graph binary OR operations"
    testList name (patternGraphOrOps |> gotExpTwinVarTests)