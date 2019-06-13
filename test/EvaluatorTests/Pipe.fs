module Bilbo.Tests.EvaluatorTests.Pipe

open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

let orPipe = [
    nodes + """
    def addNode(g) = match g | [a] => become [a, "newNode"::200]
    def removeNode(g) = match g | [a] => become []
    got = [na,nb] >> addNode <|> removeNode
    exp = [na,nb,"newNode"::200]
    ""","Or pipe of two transform, both match. Ensure result of first is retruned."

    nodes + """
    def addNode(g) = match g | [a] => become [a, "newNode"::200]
    def removeEdge(g) = match g | [a,>,b] => become [a,b]
    got = [na,nb] >> addNode <|> removeEdge
    exp = [na,nb,"newNode"::200]
    ""","Or pipe of two transforms, only first matches. Ensure result of first is returned."

    nodes + """
    def addNode(g) = match g | [a] => become [a, "newNode"::200]
    def removeEdge(g) = match g | [a,>,b] => become [a,b]
    got = [na,nb] >> removeEdge <|> addNode
    exp = [na,nb,"newNode"::200]
    ""","Or pipe of two transforms, only second matches. Ensure result of second is returned."

    nodes + """
    def addNode(g) = match g | [a] => become [a, "newNode"::200]
    def removeEdge(g) = match g | [a,>,b] => become [a,b]
    def removeDoubleEdge(g) = match g | [a,<>,b] => become [a,b]
    got = [na,nb] >> removeDoubleEdge <|> removeEdge <|> addNode
    exp = [na,nb,"newNode"::200]
    ""","Or pipe of three transforms, only third matches. Ensure result of third is returned."

    nodes + """
    def addNode(g) = match g | [a] => become [a, "newNode"::200]
    def removeEdge(g) = match g | [a,>,b] => become [a,b]
    def addEdge(g) = match g | [a,b] => become [a,<>,b]
    p = (addNode |> removeEdge) <|> addEdge
    got = [nc,nd] >> p
    exp = [nc,<>,nd]
    ""","A pipeline or piped. First stage of pipeline matches, but later one does not."
    + "Ensure original graph is piped to RHS of or pipe."

    nodes + """
    def addWeight(g,w) = match g | [a,>,b] => become [a,w>,b]
    def incWeight(g,w) = match g | [a,x>,b] => become [a,x+w>,b]
    got = ([nb,10>,ne],17.5) >> incWeight <|> addWeight
    exp = [nb,27.5>,ne]
    ""","Two arg transforms either side of or pipe. First matches. Ensure first is passed both args"

    nodes + """
    def addWeight(g,w) = match g | [a,>,b] => become [a,w>,b]
    def incWeight(g,w) = match g | [a,x>,b] => become [a,x+w>,b]
    p = incWeight <|> addWeight
    p1 = [nb,10>,ne] >> p
    got = 17.5 >> p1
    exp = [nb,27.5>,ne]
    ""","Partial application of two arg transforms either side of or pipe. First matches. Ensure first is passed both args"

    nodes + """
    def addWeight(g,w) = match g | [a,>,b] => become [a,w>,b]
    def incWeight(g,w) = match g | [a,x>,b] => become [a,x+w>,b]
    got = ([nb,>,ne],17.5) >> incWeight <|> addWeight
    exp = [nb,17.5>,ne]
    ""","Two arg transforms either side of or pipe. First fails. Ensure second is passed both args"

    nodes + """
    def addWeight(g,w) = match g | [a,>,b] => become [a,w>,b]
    def incWeight(g,w) = match g | [a,x>,b] => become [a,x+w>,b]
    p = incWeight <|> addWeight
    p1 = [nb,>,ne] >> p
    got = 17.5 >> p1
    exp = [nb,17.5>,ne]
    ""","Partial application of two arg transforms with or pipe. First fails. Ensure second is passed both args"
]

let andPipe = [
    nodes + """
    def addEdge(g) = match g | [a,b] => become [a,>,b]
    def addSelfEdge(g) = match g | [a] => become [a,>,a]
    g = [na,nb]
    got = g >> addEdge <&> addSelfEdge
    exp =
        [na,>,nb]
        |&| [na,<,nb]
        |&| [na,>,na,nb]
        |&| [na,nb,>,nb]
    ""","Single arg transforms connected using an and pipe"

    nodes + """
    def addEdge(g,w) = match g | [a,b] => become [a,w>,b]
    def addSelfEdge(g,w) = match g | [a] => become [a,w>,a] 
    got = ([na,nb],20) >> addEdge <&> addSelfEdge
    exp =
        [na,20>,nb]
        |&| [na,<20,nb]
        |&| [na,20>,na,nb]
        |&| [na,nb,20>,nb]
    ""","Multiple arg transforms connected using an and pipe"

    nodes + """
    def addEdge(g,w) = match g | [a,b] => become [a,w>,b]
    def addSelfEdge(g,w) = match g | [a] => become [a,w>,a]
    g = [na,nb]
    p = g >> addEdge <&> addSelfEdge
    got = "hello" >> p
    exp =
        [na,"hello">,nb]
        |&| [na,<"hello",nb]
        |&| [na,"hello">,na,nb]
        |&| [na,nb,"hello">,nb]
    ""","Multiple arg transforms connected using an and pipe and applied partially"
]

[<Tests>]
let test =
    // These tests contain pipelines constructed using or pipes
    let name = "Pipelines with or pipes"
    testList name (orPipe |> gotExpTwinVarTests)

[<Tests>]
let test2 =
    // These tests contain pipelines constructed using and pipes
    let name = "Pipelines with and pipes"
    testList name (andPipe |> gotExpTwinVarTests)
    