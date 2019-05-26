module Bilbo.Tests.EvaluatorTests.Pipe

open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

let orPipe = [
    nodes + """
    def addNode(g) = match g | [a] -> become [a, "newNode"::200]
    def removeNode(g) = match g | [a] -> become []
    a = [na,nb] >> addNode <|> removeNode
    b = [na,nb,"newNode"::200]
    ""","Or pipe of two transform, both match. Ensure result of first is retruned."

    nodes + """
    def addNode(g) = match g | [a] -> become [a, "newNode"::200]
    def removeEdge(g) = match g | [a,>,b] -> become [a,b]
    a = [na,nb] >> addNode <|> removeEdge
    b = [na,nb,"newNode"::200]
    ""","Or pipe of two transforms, only first matches. Ensure result of first is returned."

    nodes + """
    def addNode(g) = match g | [a] -> become [a, "newNode"::200]
    def removeEdge(g) = match g | [a,>,b] -> become [a,b]
    a = [na,nb] >> removeEdge <|> addNode
    b = [na,nb,"newNode"::200]
    ""","Or pipe of two transforms, only second matches. Ensure result of second is returned."

    nodes + """
    def addNode(g) = match g | [a] -> become [a, "newNode"::200]
    def removeEdge(g) = match g | [a,>,b] -> become [a,b]
    def removeDoubleEdge(g) = match g | [a,<>,b] -> become [a,b]
    a = [na,nb] >> removeDoubleEdge <|> removeEdge <|> addNode
    b = [na,nb,"newNode"::200]
    ""","Or pipe of three transforms, only third matches. Ensure result of third is returned."

    nodes + """
    def addNode(g) = match g | [a] -> become [a, "newNode"::200]
    def removeEdge(g) = match g | [a,>,b] -> become [a,b]
    def addEdge(g) = match g | [a,b] -> become [a,<>,b]
    p = (addNode |> removeEdge) <|> addEdge
    a = [nc,nd] >> p
    b = [nc,<>,nd]
    ""","A pipeline or piped. First stage of pipeline matches, but later one does not."
    + "Ensure original graph is piped to RHS of or pipe."

    nodes + """
    def addWeight(g,w) = match g | [a,>,b] -> become [a,w>,b]
    def incWeight(g,w) = match g | [a,x>,b] -> become [a,x+w>,b]
    a = ([nb,10>,ne],17.5) >> incWeight <|> addWeight
    b = [nb,27.5>,ne]
    ""","Two arg transforms either side of or pipe. First matches. Ensure first is passed both args"

    nodes + """
    def addWeight(g,w) = match g | [a,>,b] -> become [a,w>,b]
    def incWeight(g,w) = match g | [a,x>,b] -> become [a,x+w>,b]
    p = incWeight <|> addWeight
    p1 = [nb,10>,ne] >> p
    a = 17.5 >> p1
    b = [nb,27.5>,ne]
    ""","Partial application of two arg transforms either side of or pipe. First matches. Ensure first is passed both args"

    nodes + """
    def addWeight(g,w) = match g | [a,>,b] -> become [a,w>,b]
    def incWeight(g,w) = match g | [a,x>,b] -> become [a,x+w>,b]
    a = ([nb,>,ne],17.5) >> incWeight <|> addWeight
    b = [nb,17.5>,ne]
    ""","Two arg transforms either side of or pipe. First fails. Ensure second is passed both args"

    nodes + """
    def addWeight(g,w) = match g | [a,>,b] -> become [a,w>,b]
    def incWeight(g,w) = match g | [a,x>,b] -> become [a,x+w>,b]
    p = incWeight <|> addWeight
    p1 = [nb,>,ne] >> p
    a = 17.5 >> p1
    b = [nb,17.5>,ne]
    ""","Partial application of two arg transforms with or pipe. First fails. Ensure second is passed both args"
]

[<Tests>]
let test =
    // These tests contain pipelines constructed using or pipes
    let name = "Pipelines with or pipes"
    testList name (orPipe |> abTwinVarTests)