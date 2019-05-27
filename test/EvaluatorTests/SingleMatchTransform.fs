module Bilbo.Tests.EvaluatorTests.SingleMatchTransform

open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

let smallPatternReturnTransforms = [
    nodes + """
    def empty(g) = match g | [] => return 100
    a = [na] >> empty
    b = 100
    """, "Match on empty subgraph in non-empty host, return int";

    nodes + """
    def empty(g) = match g | [] => return 100
    a = [] >> empty
    b = 100
    """, "Match on empty subgraph in empty host, return int";

    """
    def node(g) = match g | [x] => return x
    a = ["aNode"::23984] >> node
    b = "aNode"::23984
    """, "Match on single node, return it";

    """
    def number(g) = match g | [x] => return 2.7182818
    a = ["node"::"load"] >> number
    b = 2.7182818
    """, "Match on single node, return a literal";

    """
    def number(g) =
        val = 2.7182818
        match g
        | [x] => return val
    a = ["node"::"load"] >> number
    b = 2.7182818
    """, "Match on single node, return a value defined in pre-match body";

    """
    def number(g) =
        val = 2.7182818
        match g
        | [x] =>
            val = 3.14159
            return val
    a = ["node"::"load"] >> number
    b = 3.14159
    """, "Match on single node, return a value defined in match-case body";

    nodes + """
    val = 2.7182818
    def number(g) = match g | [x] => return val
    a = [na] >> number
    b = 2.7182818
    """, "Match on single node, return value from function closure";
]

let oneParamReturnTransforms = [

    nodes + """
    def source(g) = match g | [a,>,b] => return a
    a = [na,>,nb] >> source
    b = na
    """, "Match on single edge, return source"

    nodes + """
    def wedge(g) = match g | [a,w>,b] => return w
    a = [na,200>,nb] >> wedge
    b = 200
    """, "Match on single weighted edge, return weight"

    nodes + """
    def midIn(g) = match g | [x,w>,y,<,z] => return y
    a = [na,10>,nb,<,nc] >> midIn
    b = nb
    """, "Match on 3 node pattern with edges in both directions";

    nodes + """
    def midOut(g) = match g | [x,<w,y,>,z] => return y
    a = [na,>,nb] + [nc,<10,na] >> midOut
    b = na
    """, "Graph addition enpiped into match on 3 node pattern with edges in both directions";

    nodes + """
    def last3(g) = match g | [a,>,b,>,c,>,d,>,e,>,f] => return [d,e,f]
    a = [na,>,nb,>,nc,>,nd,>,ne,>,nf] >> last3
    b = [nd,ne,nf]
    """, "6 node match, returning graphs";

    nodes + """
    def right(g) = match g | [a,>,b] => return [b]
    def loop(g) = match g | [b] => return [b,2000>,b]
    a = [na,>,nb] >> right |> loop
    b = [nb,2000>,nb]
    """, "Two transforms in pipeline, return graph from both.";

    nodes + """
    def weight(g) = match g | [r,s>,t] => return s
    b = 200
    a = [na,b>,nb] >> weight
    """, "Return weight found in subgraph.";

    nodes + """
    def biWeight(g) = match g | [a,<x>,b,>,c] => return x
    b = 200
    a = [nb,<b>,nc,>,nd] >> biWeight
    """, "Return weight, uses <> in pattern graph.";

    nodes + """
    def add100(n) = return n+100
    def weightAdd(g) = match g | [a,x>,b] => return x >> add100
    c = 200
    a = [na,c>,nb] >> weightAdd
    b = c >> add100
    """, "Return weight found in subgraph and piped through function.";

    nodes + """
    def add100(n) = return n+100
    def weight(g) = match g | [a,x>,b] => return x >> add100
    c = 200
    a = [na,c>,nb] >> weight >> add100
    b = c >> add100 >> add100
    """, "Return weight found in subgraph and piped through 2 stage pipeline.";

    nodes + """
    newNode = "NewNode"::29384
    def changeNode(g) = match g | [a,>,b,>,c] => return [a,>,newNode,>,c]
    a = [nd,>,nb] + [nb,>,na] >> changeNode
    b = [na,<,newNode,<,nd] 
    """, "Returning graph where node is swapped for a different node captured by closure"
]

let singleMatchMultipleParamTransformTests = [
    nodes + """
    def weightAddN(n,g) = match g | [a,x>,b] => return x+n
    a = 2234
    g = [nc,a-10>,nd,ne]
    b = (10,g) >> weightAddN
    """, "Multiple param transform, uses non-graph param in return, transform is never partial.";

    nodes + """
    def weightAddN(n,g) = match g | [a,x>,b] => return x+n
    a = 2234
    n = 86312
    t = 86312 >> weightAddN
    g = [nc,a-n>,nd,ne]
    b = g >> t
    """, "Multiple param transform, uses non-graph param in return, transform applied partially."

    nodes + """
    def incWeight(g,n) = match g | [a,x>,b] => return [a,x+n>,b]
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
    def tran(g) = match g | [a,>,b,>,a,>,c] => return b
    a = [na,>,nb,>,na,>,nc] >> tran
    b = nb
    """, "Node matches twice in pattern. I";

    nodes + """
    def tran(g) = match g | [a,>,b,>,a,>,c] => return b
    a = [nb,>,nc,>,nb,>,nd] >> tran
    b = nc
    """, "Node matches twice in pattern. II";

    nodes + """
    def tran(g) = match g | [a,<>,b,>,c] => return b
    a = [nc,<>,nd,>,na] >> tran
    b = nd
    """, "Node matches twice in pattern. III"
]

let singleMatchBecomeTests = [
    nodes + """
    def swap(g) = match g | [a,>,b] => become [a,<,b]
    g = [na,>,nb]
    a = g >> swap
    b = [na,<,nb]
    """, "Become test. No other edges or nodes. Equivalent to return."

    nodes + """
    def swap(g) = match g | [a,>,b] => become [a,<,b]
    g = [na,>,nb,nc,nd]
    a = g >> swap
    b = [na,<,nb,nc,nd]
    """, "Become test. Edge swap with no other edges but other nodes."

    nodes + """
    def swapW(g) = match g | [a,x>,b] => become [a,<x,b]
    g = [na,>,nb,40>,nc,<>,nd]
    a = g >> swapW
    b = [na,>,nb,<40,nc,<>,nd]
    """, "Become test. Edge swap with other edges but no other (non-endpoint) nodes."


    nodes + """
    def swapW(g) = match g | [a,x>,b] => become [a,<x,b]
    g = [ne,nf,na,>,nb,40>,nc,<>,nd]
    a = g >> swapW
    b = [ne,nf,na,>,nb,<40,nc,<>,nd]
    """, "Become test. Edge swap with other edges and other nodes."
]

[<Tests>]
let test =
    // These tests contain transforms that match sub graphs with no nodes or a single node
    // The transforms all return rather than become
    // The transforms in these tests have no where statement, a single match case, and a single subgraph match
    let name = "Return transforms with empty and single node graph patterns"
    testList name (smallPatternReturnTransforms |> abTwinVarTests)

[<Tests>]
let test2 =
    // These tests contain transforms that require single parameter
    // The transforms all return rather than become
    // The transforms in these tests have no where statement, a single match case, and a single subgraph match
    let name = "Single parameter return transforms"
    testList name (oneParamReturnTransforms |> abTwinVarTests)


[<Tests>]
let test3 =
    let name =
        "Multiple param transforms with explict matching."
        + "No where statements and a single match case that will match a single subgraph."
    testList name (singleMatchMultipleParamTransformTests |> abTwinVarTests)

[<Tests>]
let test4 =
    let name = "Same node appears more than once in pattern graph"
    testList name (singleMatchNodeMatchesMultipleTimes |> abTwinVarTests)

[<Tests>]
let test5 =
    let name = "Single match become tests"
    testList name (singleMatchBecomeTests |> abTwinVarTests)