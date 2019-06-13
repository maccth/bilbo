module Bilbo.Tests.EvaluatorTests.Reduction

open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

let filterTests = [
    nodes + """
    def anything(g) = match g | [] => return True
    got = [] >> filter by anything
    exp = []
    """, "Ensure positive filtering works on empty graph."
    
    nodes + """
    def hasNodes(g) = match g | [x] => return True
    got = [na] >> filter by hasNodes
    exp = [na]
    """, "Ensure positive filtering works on single node graph."

    nodes + """
    def hasNodes(g) = match g | [x] => return True
    got = [na] |&| [nb] |&| [nc] >> filter by hasNodes
    exp = [na] |&| [nb] |&| [nc]
    """, "Ensure positive filtering works on a collection of single node graphs."

    nodes + """
    def hasSelfLoop(g) = match g | [a,>,a] => return True 
    g = [na,>,na] |&| [na,>,nb,>,nc]
    got = g >> filter by hasSelfLoop
    exp = [na,>,na]
    """, "Ensure no match favours deletion."

    nodes + """
    def hasSelfLoop(g) = match g | [a,>,a] => return True
    def doubleEdge(g) = match g | [x,>,x] and not [x,<>,x] => become [x,<>,x]
    g = [na,>,na] |&| [na,>,nb,>,nc]
    got = g >> filter by hasSelfLoop |> doubleEdge!
    exp = [na,<>,na]
    """, "Ensure filtering works at the start of a pipeline."

    nodes + """
    def hasSelfLoop(g) = match g | [a,>,a] => return True
    def doubleEdge(g) = match g | [x,>,x] and not [x,<>,x] => become [x,<>,x]
    g = [na,>,na] |&| [na,>,nb,>,nc]
    got = g >> filter by hasSelfLoop |> doubleEdge!
    exp = [na,<>,na]
    """, "Ensure filtering works at the end of a pipeline."

    nodes + """
    def hasNoSelfLoop(g) =
        match g
        | [x,>,x]   => return False
        | _         => return True
    g = [na,>,na,>,nb] |&| [na,>,nb,>,nc] |&| [na,<,na]
    got = g >> filter by hasNoSelfLoop
    exp = [na,>,nb,>,nc]
    """, "Ensure negative questions can be answered due to the all Truth requirement."

    """
    def hasWeightLargerThan(g,x) = match g | [n] where #n > x => return True
    na = "a"::10
    nb = "b"::20
    g = [na] |&| [nb]
    g' = g >> filter by hasWeightLargerThan
    got = 12 >> g'
    exp = [nb] 
    ""","Ensure partially applied filters work"
]

let minTests = [
    """
    def load(g) = match g | [x] => return #x
    na = "a"::100
    g = [na]
    got = g >> min by load
    exp = [na]
    ""","Ensure min reduction with a single graph returns that same graph."

    """
    def load(g) = match g | [x] => return #x
    na = "a"::100
    nb = "b"::200
    g = [na] |&| [nb]
    got = g >> min by load
    exp = [na]
    ""","Ensure min reduction works on single node graphs."

    """
    def load(g) = match g | [x] => return #x
    na = "a"::100
    nb = "b"::200
    nc = "c"::300
    g = [nc,>,na] |&| [nb,<,nc]
    got = g >> min by load
    exp = [nc,>,na]
    ""","Ensure min reduction picks the min within every graph, if there are multiple matches."
]

let maxTests = [
    """
    def load(g) = match g | [x] => return #x
    na = "a"::100
    g = [na]
    got = g >> max by load
    exp = [na]
    ""","Ensure max reduction with a single graph returns that same graph."

    """
    def load(g) = match g | [x] => return #x
    na = "a"::100
    nb = "b"::200
    g = [na] |&| [nb]
    got = g >> max by load
    exp = [nb]
    ""","Ensure max reduction works on single node graphs."

    """
    def load(g) = match g | [x] => return #x
    na = "a"::100
    nb = "b"::200
    nc = "c"::300
    g = [nc,>,na] |&| [nb,<,na]
    got = g >> max by load
    exp = [nc,>,na]
    ""","Ensure max reduction picks the max within every graph, if there are multiple matches."
]

[<Tests>]
let test =
    let name = "Filter reduction tests"
    testList name (filterTests |> abTwinVarTests)


[<Tests>]
let test2 =
    let name = "Min reduction tests"
    testList name (minTests |> abTwinVarTests)

[<Tests>]
let test3 =
    let name = "Max reduction tests"
    testList name (minTests |> abTwinVarTests)