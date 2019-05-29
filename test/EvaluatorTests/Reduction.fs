module Bilbo.Tests.EvaluatorTests.Reduction

open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

let filterTests = [
    nodes + """
    def anything(g) = match g | [] => return True
    a = [] >> filter by anything
    b = []
    """, "Ensure positive filtering works on empty graph."
    
    nodes + """
    def hasNodes(g) = match g | [x] => return True
    a = [na] >> filter by hasNodes
    b = [na]
    """, "Ensure positive filtering works on single node graph."

    nodes + """
    def hasNodes(g) = match g | [x] => return True
    a = [na] |&| [nb] |&| [nc] >> filter by hasNodes
    b = [na] |&| [nb] |&| [nc]
    """, "Ensure positive filtering works on a collection of single node graphs."

    nodes + """
    def hasSelfLoop(g) = match g | [a,>,a] => return True 
    g = [na,>,na] |&| [na,>,nb,>,nc]
    a = g >> filter by hasSelfLoop
    b = [na,>,na]
    """, "Ensure no match favours deletion."

    nodes + """
    def hasSelfLoop(g) = match g | [a,>,a] => return True
    def doubleEdge(g) = match g | [x,>,x] and not [x,<>,x] => become [x,<>,x]
    g = [na,>,na] |&| [na,>,nb,>,nc]
    a = g >> filter by hasSelfLoop |> doubleEdge!
    b = [na,<>,na]
    """, "Ensure filtering works at the start of a pipeline."

    nodes + """
    def hasSelfLoop(g) = match g | [a,>,a] => return True
    def doubleEdge(g) = match g | [x,>,x] and not [x,<>,x] => become [x,<>,x]
    g = [na,>,na] |&| [na,>,nb,>,nc]
    a = g >> filter by hasSelfLoop |> doubleEdge!
    b = [na,<>,na]
    """, "Ensure filtering works at the end of a pipeline."

    nodes + """
    def hasNoSelfLoop(g) =
        match g
        | [x,>,x]   => return False
        | _         => return True
    g = [na,>,na,>,nb] |&| [na,>,nb,>,nc] |&| [na,<,na]
    a = g >> filter by hasNoSelfLoop
    b = [na,>,nb,>,nc]
    """, "Ensure negative questions can be answered due to the all Truth requirement."

    """
    def hasWeightLargerThan(g,x) = match g | [n] where #n > x => return True
    na = "a"::10
    nb = "b"::20
    g = [na] |&| [nb]
    g' = g >> filter by hasWeightLargerThan
    a = 12 >> g'
    b = [nb] 
    ""","Ensure partially applied filters work"
]

let minTests = [
    """
    def load(g) = match g | [x] => return #x
    na = "a"::100
    g = [na]
    a = g >> min by load
    b = [na]
    ""","Ensure min reduction with a single graph returns that same graph."

    """
    def load(g) = match g | [x] => return #x
    na = "a"::100
    nb = "b"::200
    g = [na] |&| [nb]
    a = g >> min by load
    b = [na]
    ""","Ensure min reduction works on single node graphs."

    """
    def load(g) = match g | [x] => return #x
    na = "a"::100
    nb = "b"::200
    nc = "c"::300
    g = [nc,>,na] |&| [nb,<,nc]
    a = g >> min by load
    b = [nc,>,na]
    ""","Ensure min reduction picks the min within every graph, if there are multiple matches."
]

let maxTests = [
    """
    def load(g) = match g | [x] => return #x
    na = "a"::100
    g = [na]
    a = g >> max by load
    b = [na]
    ""","Ensure max reduction with a single graph returns that same graph."

    """
    def load(g) = match g | [x] => return #x
    na = "a"::100
    nb = "b"::200
    g = [na] |&| [nb]
    a = g >> max by load
    b = [nb]
    ""","Ensure max reduction works on single node graphs."

    """
    def load(g) = match g | [x] => return #x
    na = "a"::100
    nb = "b"::200
    nc = "c"::300
    g = [nc,>,na] |&| [nb,<,na]
    a = g >> max by load
    b = [nc,>,na]
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