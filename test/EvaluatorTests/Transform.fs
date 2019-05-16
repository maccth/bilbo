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
        | [x,>,y,<,z] -> return y
"""

let retMidOut = """
    def midOut(g) =
        match g
        | [x,<,y,>,z] -> return y
"""

let retWeight = "def weight(g) = match g | [r,s>,t] -> return s"

let add100 = "def add100(n) = return n+100"

let singleMatchSingleParamTransformTests = [
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
    a = [na,>,nb,<,nc] >> midIn
    b = nb
    """, "Match on 3 node pattern with edges in both directions";

    nodes + retMidOut + """
    a = [na,>,nb] + [nc,<,na] >> midOut
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
]

let quickTwinVarTest codeStr des =
    (codeStr, "a", "b", des) 

let quickTwinVarTests tLst =
    tLst
    |> List.map (fun (c,d) -> quickTwinVarTest c d)
    |> twinVarTests

[<Tests>]
let test =
    let name =
        "Single param transforms with explict matching."
        + "No where statements and a single match case that will match a single subgraph."
    testList name (singleMatchSingleParamTransformTests |> quickTwinVarTests)