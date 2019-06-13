module Bilbo.Tests.EvaluatorTests.SingleMatchTransform

open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

let smallPatternReturnTransforms = [
    nodes + """
    def empty(g) = match g | [] => return 100
    got = [na] >> empty
    exp = 100
    """, "Match on empty subgraph in non-empty host, return int";

    nodes + """
    def empty(g) = match g | [] => return 100
    got = [] >> empty
    exp = 100
    """, "Match on empty subgraph in empty host, return int";

    """
    def node(g) = match g | [x] => return x
    got = ["aNode"::23984] >> node
    exp = "aNode"::23984
    """, "Match on single node, return it";

    """
    def number(g) = match g | [x] => return 2.7182818
    got = ["node"::"load"] >> number
    exp = 2.7182818
    """, "Match on single node, return a literal";

    """
    def number(g) =
        val = 2.7182818
        match g
        | [x] => return val
    got = ["node"::"load"] >> number
    exp = 2.7182818
    """, "Match on single node, return a value defined in pre-match body";

    """
    def number(g) =
        val = 2.7182818
        match g
        | [x] =>
            val = 3.14159
            return val
    got = ["node"::"load"] >> number
    exp = 3.14159
    """, "Match on single node, return a value defined in match-case body";

    nodes + """
    val = 2.7182818
    def number(g) = match g | [x] => return val
    got = [na] >> number
    exp = 2.7182818
    """, "Match on single node, return value from function closure";
]

let oneParamReturnTransforms = [

    nodes + """
    def source(g) = match g | [a,>,b] => return a
    got = [na,>,nb] >> source
    exp = na
    """, "Match on single edge, return source"

    nodes + """
    def wedge(g) = match g | [a,w>,b] => return w
    got = [na,200>,nb] >> wedge
    exp = 200
    """, "Match on single weighted edge, return weight"

    nodes + """
    def midIn(g) = match g | [x,w>,y,<,z] => return y
    got = [na,10>,nb,<,nc] >> midIn
    exp = nb
    """, "Match on 3 node pattern with edges in both directions";

    nodes + """
    def midOut(g) = match g | [x,<w,y,>,z] => return y
    got = [na,>,nb] + [nc,<10,na] >> midOut
    exp = na
    """, "Graph addition enpiped into match on 3 node pattern with edges in both directions";

    nodes + """
    def last3(g) = match g | [a,>,b,>,c,>,d,>,e,>,f] => return [d,e,f]
    got = [na,>,nb,>,nc,>,nd,>,ne,>,nf] >> last3
    exp = [nd,ne,nf]
    """, "6 node match, returning graphs";

    nodes + """
    def right(g) = match g | [a,>,b] => return [b]
    def loop(g) = match g | [b] => return [b,2000>,b]
    got = [na,>,nb] >> right |> loop
    exp = [nb,2000>,nb]
    """, "Two transforms in pipeline, return graph from both.";

    nodes + """
    def weight(g) = match g | [r,s>,t] => return s
    exp = 200
    got = [na,exp>,nb] >> weight
    """, "Return weight found in subgraph.";

    nodes + """
    def biWeight(g) = match g | [a,<x>,b,>,c] => return x
    exp = 200
    got = [nb,<exp>,nc,>,nd] >> biWeight
    """, "Return weight, uses <> in pattern graph.";

    nodes + """
    def add100(n) = return n+100
    def weightAdd(g) = match g | [a,x>,b] => return x >> add100
    c = 200
    got = [na,c>,nb] >> weightAdd
    exp = c >> add100
    """, "Return weight found in subgraph and piped through function.";

    nodes + """
    def add100(n) = return n+100
    def weight(g) = match g | [a,x>,b] => return x >> add100
    c = 200
    got = [na,c>,nb] >> weight >> add100
    exp = c >> add100 >> add100
    """, "Return weight found in subgraph and piped through 2 stage pipeline.";

    nodes + """
    newNode = "NewNode"::29384
    def changeNode(g) = match g | [a,>,b,>,c] => return [a,>,newNode,>,c]
    got = [nd,>,nb] + [nb,>,na] >> changeNode
    exp = [na,<,newNode,<,nd] 
    """, "Returning graph where node is swapped for a different node captured by closure"
]

let singleMatchMultipleParamTransformTests = [
    nodes + """
    def weightAddN(n,g) = match g | [a,x>,b] => return x+n
    got = 2234
    g = [nc,got-10>,nd,ne]
    exp = (10,g) >> weightAddN
    """, "Multiple param transform, uses non-graph param in return, transform is never partial.";

    nodes + """
    def weightAddN(n,g) = match g | [a,x>,b] => return x+n
    got = 2234
    n = 86312
    t = 86312 >> weightAddN
    g = [nc,got-n>,nd,ne]
    exp = g >> t
    """, "Multiple param transform, uses non-graph param in return, transform applied partially."

    nodes + """
    def incWeight(g,n) = match g | [a,x>,b] => return [a,x+n>,b]
    g = [na,100>,nb]
    p = g >> incWeight |> incWeight
    p' = 20 >> p
    got = 30 >> p'
    exp = [na,100+20+30>,nb]
    """, "Pipeline of multiple param transforms, applied partially"
]

let singleMatchWithWhere = [
    nodes + """
    def weight(g) = match g | [a,x>,b] where x>10
    g = [na,7>,nb]
    got = g >> weight
    """, "Should not match. Where clause fails"
]

let singleMatchNodeMatchesMultipleTimes = [
    nodes + """
    def tran(g) = match g | [a,>,b,>,a,>,c] => return b
    got = [na,>,nb,>,na,>,nc] >> tran
    exp = nb
    """, "Node matches twice in pattern. I";

    nodes + """
    def tran(g) = match g | [a,>,b,>,a,>,c] => return b
    got = [nb,>,nc,>,nb,>,nd] >> tran
    exp = nc
    """, "Node matches twice in pattern. II";

    nodes + """
    def tran(g) = match g | [a,<>,b,>,c] => return b
    got = [nc,<>,nd,>,na] >> tran
    exp = nd
    """, "Node matches twice in pattern. III"
]

let singleMatchBecomeTests = [
    nodes + """
    def swap(g) = match g | [a,>,b] => become [a,<,b]
    g = [na,>,nb]
    got = g >> swap
    exp = [na,<,nb]
    """, "Become test. No other edges or nodes. Equivalent to return."

    nodes + """
    def swap(g) = match g | [a,>,b] => become [a,<,b]
    g = [na,>,nb,nc,nd]
    got = g >> swap
    exp = [na,<,nb,nc,nd]
    """, "Become test. Edge swap with no other edges but other nodes."

    nodes + """
    def swapW(g) = match g | [a,x>,b] => become [a,<x,b]
    g = [na,>,nb,40>,nc,<>,nd]
    got = g >> swapW
    exp = [na,>,nb,<40,nc,<>,nd]
    """, "Become test. Edge swap with other edges but no other (non-endpoint) nodes."


    nodes + """
    def swapW(g) = match g | [a,x>,b] => become [a,<x,b]
    g = [ne,nf,na,>,nb,40>,nc,<>,nd]
    got = g >> swapW
    exp = [ne,nf,na,>,nb,<40,nc,<>,nd]
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