module Bilbo.Tests.EvaluatorTests.Transform

open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

let nodes = """
na = "nodeA"::1001
nb = "nodeB"::1002
nc = "nodeC"::1003
nd = "nodeD"::1004
ne = "nodeE"::1005
nf = "nodeF"::1006
"""

let retNode = """
    def node(g) =
        match g
        | [x] -> return x
"""

let retMidIn = """
    def midIn(g) =
        match g
        | [x,w>,y,<,z] -> return y
"""

let retMidOut = """
    def midOut(g) = match g | [x,<w,y,>,z] -> return y
"""

let retWeight = """
    def weight(g) = match g | [r,s>,t] -> return s
"""

let retWeightAddN = """
    def weightAddN(n,g) = match g | [a,x>,b] -> return x+n
"""

let add100 = """
    def add100(n) = return n+100
"""

let becSwap = """
    def swap(g) = match g | [a,>,b] -> become [a,<,b]
"""

let becSwapW = """
    def swapW(g) = match g | [a,x>,b] -> become [a,<x,b]
"""

let becDelNode = """
    def delNode(g) = match g | [x] -> become []
"""

let singleMatchSingleParamTransformTests = [
    nodes + """
    def empty(g) = match g | [] -> return 100
    a = 100
    b = [na] >> empty
    """, "Match on empty graph, return int";

    retNode + """
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

    nodes + """
    a = 2.7182818
    def number(g) =
        match g
        | [x] -> return a
    b = [na] >> number
    """, "Match on single node, return value from function closure";

    nodes + retMidIn + """
    a = [na,10>,nb,<,nc] >> midIn
    b = nb
    """, "Match on 3 node pattern with edges in both directions";

    nodes + retMidOut + """
    a = [na,>,nb] + [nc,<10,na] >> midOut
    b = na
    """, "Graph addition enpiped into match on 3 node pattern with edges in both directions";

    nodes + """
    def last3(g) = match g | [a,>,b,>,c,>,d,>,e,>,f] -> return [d,e,f]
    a = [na,>,nb,>,nc,>,nd,>,ne,>,nf] >> last3
    b = [nd,ne,nf]
    """, "6 node match, returning graphs";

    nodes + """
    def right(g) = match g | [a,>,b] -> return [b]
    def loop(g) = match g | [b] -> return [b,2000>,b]
    a = [na,>,nb] >> right |> loop
    b = [nb,2000>,nb]
    """, "Two transforms in pipeline, return graph from both.";

    nodes + retWeight + """
    b = 200
    a = [na,b>,nb] >> weight
    """, "Return weight found in subgraph.";

    nodes + """
    def biWeight(g) = match g | [a,<x>,b,>,c] -> return x
    b = 200
    a = [nb,<b>,nc,>,nd] >> biWeight
    """, "Return weight, uses <> in pattern graph.";

    nodes + add100 + """
    def weightAdd(g) = match g | [a,x>,b] -> return x >> add100
    c = 200
    a = [na,c>,nb] >> weightAdd
    b = c >> add100
    """, "Return weight found in subgraph and piped through function.";

    nodes + add100 + """
    def weight(g) = match g | [a,x>,b] -> return x >> add100
    c = 200
    a = [na,c>,nb] >> weight >> add100
    b = c >> add100 >> add100
    """, "Return weight found in subgraph and piped through 2 stage pipeline.";

    nodes + """
    newNode = "NewNode"::29384
    def changeNode(g) = match g | [a,>,b,>,c] -> return [a,>,newNode,>,c]
    a = [nd,>,nb] + [nb,>,na] >> changeNode
    b = [na,<,newNode,<,nd] 
    """, "Returning graph where node is swapped for a different node captured by closure"
]

let singleMatchMultipleParamTransformTests = [
    nodes + retWeightAddN + """
    a = 2234
    g = [nc,a-10>,nd,ne]
    b = (10,g) >> weightAddN
    """, "Multiple param transform, uses non-graph param in return, transform is never partial.";

    nodes + retWeightAddN  + """
    a = 2234
    n = 86312
    t = 86312 >> weightAddN
    g = [nc,a-n>,nd,ne]
    b = g >> t
    """, "Multiple param transform, uses non-graph param in return, transform applied partially."

    nodes + """
    def incWeight(g,n) = match g | [a,x>,b] -> return [a,x+n>,b]
    g = [na,100>,nb]
    p = g >> incWeight |> incWeight
    p' = 20 >> p
    a = 30 >> p'
    b = [na,100+20+30>,nb]
    """, "Pipeline of multiple param transforms, applied partially"
]

let singleMatchWithWhere = [
    nodes + """
    def weight(g) = match g | [a,x>,b] where x>10
    g = [na,7>,nb]
    a = g >> weight
    """, "Should not match. Where clause fails"
]

let singleMatchNodeMatchesMultipleTimes = [
    nodes + """
    def tran(g) = match g | [a,>,b,>,a,>,c] -> return b
    a = [na,>,nb,>,na,>,nc] >> tran
    b = nb
    """, "Node matches twice in pattern. I";

    nodes + """
    def tran(g) = match g | [a,>,b,>,a,>,c] -> return b
    a = [nb,>,nc,>,nb,>,nd] >> tran
    b = nc
    """, "Node matches twice in pattern. II";

    nodes + """
    def tran(g) = match g | [a,<>,b,>,c] -> return b
    a = [nc,<>,nd,>,na] >> tran
    b = nd
    """, "Node matches twice in pattern. III"
]

let singleMatchBecomeTests = [
    nodes + becSwap + """
    g = [na,>,nb]
    a = g >> swap
    b = [na,<,nb]
    """, "Become test. No other edges or nodes. Equivalent to return."

    nodes + becSwap + """
    g = [na,>,nb,nc,nd]
    a = g >> swap
    b = [na,<,nb,nc,nd]
    """, "Become test. Edge swap with no other edges but other nodes."

    nodes + becSwapW + """
    g = [na,>,nb,40>,nc,<>,nd]
    a = g >> swapW
    b = [na,>,nb,<40,nc,<>,nd]
    """, "Become test. Edge swap with other edges but no other (non-endpoint) nodes."


    nodes + becSwapW + """
    g = [ne,nf,na,>,nb,40>,nc,<>,nd]
    a = g >> swapW
    b = [ne,nf,na,>,nb,<40,nc,<>,nd]
    """, "Become test. Edge swap with other edges and other nodes."
]

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
]

let collectionTests = [
    nodes + becDelNode + """
    g = [na,nb]
    a = g >> delNode
    b = [na] |&| [nb]
    """, "Deleting a node resulting in a collection of two graphs. Ensure both graphs are returned in a collection."

    nodes + becDelNode + """
    g = [na,nb]
    a = g >> delNode |> delNode
    b = [] |&| []
    """, "Deleting a node twice. Returns a collection of two empty graphs. Ensure that these are both retruned."

    nodes + becDelNode + """
    g = [na,>,nb,>,nc]
    a = g >> delNode
    b =
        [nb,>,nc]
        |&| [na,nc]
        |&| [na,>,nb]
    """, "Deleting a node from a graph with multiple nodes. Ensure that edge deletion/preservation on works correctly on collections"
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

let quickTwinVarTest codeStr des =
    (codeStr, "a", "b", des) 

let quickTwinVarTests tLst =
    tLst
    |> List.map (fun (c,d) -> quickTwinVarTest c d)
    |> twinVarTests

[<FTests>]
let test =
    let name =
        "Single param transforms with explict matching. "
        + "No where statements and a single match case that will match a single subgraph."
    testList name (singleMatchSingleParamTransformTests |> quickTwinVarTests)

[<FTests>]
let test2 =
    let name =
        "Multiple param transforms with explict matching."
        + "No where statements and a single match case that will match a single subgraph."
    testList name (singleMatchMultipleParamTransformTests |> quickTwinVarTests)

[<FTests>]
let test3 =
    let name = "Same node appears more than once in pattern graph"
    testList name (singleMatchNodeMatchesMultipleTimes |> quickTwinVarTests)

[<FTests>]
let test4 =
    let name = "Single match become tests"
    testList name (singleMatchBecomeTests |> quickTwinVarTests)

[<FTests>]
let test5 =
    let name = "Multiple match cases tests"
    testList name (multipleMatchCasesTransformTests |> quickTwinVarTests)

[<FTests>]
let test6 =
    let name = "Tests involving collections due to multiple subgraph matches"
    testList name (collectionTests |> quickTwinVarTests)

[<FTests>]
let test7 =
    let name = "Tests involving pattern graph binary operations"
    testList name (patternGraphBinOps |> quickTwinVarTests)
   