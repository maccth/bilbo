module Bilbo.Tests.EvaluatorTests.Transform

open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

let multipleMatchCasesTransformTests = [
    nodes + """
    def graph(g) =
        match g
        | [a] -> return g
        | [] ->
            n = "startNode"::200
            return [n]
    a = [na] >> graph
    b = [na]        
    ""","Match first case, doesn't go to second."

    nodes + """
    def graph(g) =
        match g
        | [a] -> return g
        | [] ->
            n = "startNode"::200
            return [n]
    a = [] >> graph
    b = ["startNode"::200]        
    ""","Fail first case, go to second."

    nodes + """
    def graph(g) =
        match g
        | [a] -> return []
        | [a] -> return g
    a = [na] >> graph
    b = []        
    ""","Both cases the same. Ensure first is taken."

    nodes + """
    def graph(g) =
        match g
        | [a] where a == nb -> return []
        | [a] -> return g
    a = [na] >> graph
    b = [na]        
    ""","Both pattern graphs the same, but first where clause fails. Ensure second is taken."

    nodes + """
    def graph(g) =
        match g
        | [a] where a == na -> return []
        | [a] -> return g
    a = [na] >> graph
    b = []        
    ""","Both pattern graphs the same, but first where clause passes. Ensure first is taken."

    """
    def change(g) = match g | [x,>,y] ->
        x = x::20
        become [x,>,y]
    na = "a"::0
    nb = "b"::1
    nc = "c"::2
    g = [na,>,nb] + [na,>,nc]
    a = g >> change
    b = ["a"::20,>,nb] + ["a"::20,>,nc]
    """, "Changing a node in the graph. Ensure that edge deletion/preservation on works correctly when nodes are changed"

    """
    def change(g) = match g | [x,>,y] ->
        x = x::20
        become [x,30>,y]
    na = "a"::0
    nb = "b"::1
    nc = "c"::2
    g = [na,>,nb] + [na,20>,nc]
    a = g >> change
    b = ["a"::20, 30>, nb] + ["a"::20, 20>, nc]
    """, "Changing a node in the graph. Ensure that edge deletion/preservation on works correctly when nodes and edges are changed"
]

let collectionTests = [
    nodes + """
    def delNode(g) = match g | [x] -> become []
    g = [na,nb]
    a = g >> delNode
    b = [na] |&| [nb]
    """, "Deleting a node resulting in a collection of two graphs. Ensure both graphs are returned in a collection."

    nodes + """
    def delNode(g) = match g | [x] -> become []
    g = [na,nb]
    a = g >> delNode |> delNode
    b = []
    """, "Deleting a node twice. Returns a collection of two empty graphs. Ensure that these are collapsed to one graph."

    nodes + """
    def delNode(g) = match g | [x] -> become []
    g = [na,>,nb,>,nc]
    a = g >> delNode
    b =
        [nb,>,nc]
        |&| [na,nc]
        |&| [na,>,nb]
    """, "Deleting a node from a graph with multiple nodes. Ensure that edge deletion/preservation works correctly on collections"
]

let patternGraphBinOps = [
    nodes + """
    def endPair(g) =
        match g
        | [a,>,b] and not [b,>,c] -> return [a,>,b]
    g = [na,>,nb,>,nc]
    a = g >> endPair
    b = [nb,>,nc]
    """, "Negative application conditions ensure only one subgraph matches"

    nodes + """
    def endPair(g) =
        match g
        | [a,>,b] and not [b,>,c] -> return [a,>,b]
    g = [na,>,nb,>,nc] + [nd,>,ne,>,nc]
    a = g >> endPair
    b = [nb,>,nc] |&| [ne,>,nc]
    """, "Negative application conditions ensure a collection of only two graphs is returned"
]

let patternGraphOrOps = [
    nodes + """
    def edge(g) = match g
        | [a,<>,b]
        or [a,>,b] -> return [a,b]
    g = [na,<>,nb]
    a = g >> edge
    b = [na,nb]
    """, "Or application conditions in positive graph"

    nodes + """
    def deadLink(g) =
        match g
        | [a,>,b] and not ([b,>,c] or [d,>,b]) -> return [a,>,b]
    g = [na,>,nb] + [nc,>,na,<,nd] + [nc,>,nd,>,ne]
    a = g >> deadLink
    b = [na,>,nb] |&| [nd,>,ne]
    """, "Or application conditions in negative graph"
]


[<Tests>]
let test5 =
    let name = "Multiple match cases tests"
    testList name (multipleMatchCasesTransformTests |> abTwinVarTests)

[<Tests>]
let test6 =
    let name = "Tests involving collections due to multiple subgraph matches"
    testList name (collectionTests |> abTwinVarTests)

[<Tests>]
let test7 =
    let name = "Tests involving pattern graph binary operations"
    testList name (patternGraphBinOps |> abTwinVarTests)

[<Tests>]
let test8 =
    let name = "Tests involving pattern graph binary OR operations"
    testList name (patternGraphOrOps |> abTwinVarTests)