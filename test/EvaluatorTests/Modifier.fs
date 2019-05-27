module Bilbo.Tests.EvaluatorTests.Modifier

open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

let multipleApplicationFunctions = [
    """
    def addOne(x) = return x+1
    a = 10 >> addOne ** 1
    b = 11
    """, "Single param function, mulaplied once"

    """
    def addOne(x) = return x+1
    a = 10 >> addOne ** 2
    b = 12
    """, "Single param function, mulaplied by two"

    """
    def addOne(x) = return x+1
    a = 10 >> addOne ** 3
    b = 13
    """, "Single param function, mulaplied by three"

    """
    def addOne(x) = return x+1
    addThree = addOne ** 3
    a = 10 >> addThree
    b = 13
    """, "Pipeline created by mulaplying by three, enpipe into pipeline"

    """
    def addOne(x) = return x+1
    addTwo = addOne ** 2
    addThree = addTwo |> addOne
    addSix = addThree ** 2
    addTwelve = addSix |> addTwo |> addThree |> addOne
    a = 10 >> addTwelve
    b = 22
    """, "Mulaplying an already mulaplied pipeline (all single param functions)"

    """
    def dbl(x) = return x*2
    def inc(x) = return x+1
    p = dbl**2 |> inc**3 |> dbl**4 |> inc**5
    a = 3 >> p
    b = 245
    """, "Single param functions, mulaplied such that order matters. Ensures order of pipeline stages is correct"

    """
    def zero(x) = return 0
    def inc(x) = return x+1
    p = zero |> inc |> inc |> zero |> inc
    a = 8634 >> p
    b = 1
    """, "Single param functions, mulaplied such that order matters. Ensures order of pipeline stages is correct II"

    """
    def sum(x,y) = return x+y    a = (10,20) >> sum ** 1
    b = 30
    """, "Multiple param function, mulaplied once"

    """
    def sum(x,y) = return x+y
    a = (10,20,30) >> sum ** 2
    b = 60
    """, "Multiple param function, mulaplied twice with enough arguments"

    """
    def sum(x,y) = return x+y
    p = (34,9) >> sum ** 2
    a = 17 >> p
    b = 34+9+17
    """, "Multiple param function, mulaplied twice partially"

    """
    def sum(x,y) = return x+y
    sum5 = sum ** 4
    a = (34,9,38,0,4) >> sum5
    b = 34+9+38+0+4
    """, "Multiple param function, mulaplied five times, partially"
]

let multipleApplicationTransforms = [
    nodes + """
    def incWeight(g) = match g | [a,x>,b] => become [a,x+1>,b]
    a = [na,100>,nb] >> incWeight ** 1
    b = [na,101>,nb]
    """, "Single param transform mulaplied once"

    nodes + """
    def incWeight(g) = match g | [a,x>,b] => become [a,x+1>,b]
    a = [na,100>,nb] >> incWeight ** 5
    b = [na,105>,nb]
    """, "Single param transform mulaplied five times"

    nodes + """
    def incWeight(g) = match g | [a,x>,b] => become [a,x+1>,b]
    a = [na,100>,nb,0>,nc] >> incWeight ** 2
    b = [na,102>,nb,0>,nc]
        |&| [na,101>,nb,1>,nc]
        |&| [na,100>,nb,2>,nc]
    """, "Single param transform mulaplied twice but with multiple possible matches"
]

let alapTransforms = [
    nodes + """
    def join(g) = match g | [a,b] and not [a,>,b] => become [a,>,b]
    a = [na,nb] >> join!
    b = [na,<>,nb]
    ""","Single param transform ALAP application, applied twice, with single graph output"
    
    nodes + """
    def break(g) = match g | [a,>,b] => become [a,b]
    a = [na,>,nb,>,nc,>,nd] >> break!
    b = [na,nb,nc,nd]
    ""","Single param transform ALAP application, applied thrice, with single graph output"

    nodes + """
    def break(g) = match g | [a,>,b] => become [a,b]
    a = [na,<>,nb] |&| [nc,<>,nd] >> break!
    b = [na,nb] |&| [nc,nd]
    ""","Single param transform ALAP application, applied to collection, with collection output"

    nodes + """
    def join(g) = match g | [a,b] and not [a,x>,b]=> become [a,0>,b]
    def incWeight(g) =
        match g 
        | [a,x>,b] where x==0 =>
            y = x+1
            become [a,y>,b]
    a = [na,nb] >> (join |> incWeight)!
    b = [na,<1>,nb]
    ""","Two single param transforms in a pipeline applied ALAP"

    nodes + """
    def join(g) = match g | [a,b] and not [a,x>,b]=> become [a,0>,b]
    def incWeight(g) =
        match g 
        | [a,x>,b] where x==0 =>
            y = x+1
            become [a,y>,b]
    a = [na,nb] >> join! |> incWeight!
    b = [na,<1>,nb]
    ""","Two single param transforms in a pipeline each applied ALAP"
]

let dollarTransforms = [
    nodes + """
    def remEdge(g) = match g | [a,>,b] => become [a,b]
    a = [na,>,nb,>,nc] >> $remEdge
    b = [na,nb,>,nc]
    c = [na,>,nb,nc]
    """,["b"; "c"],
    "A transform with two potential matches dollar applied. Ensure answer is only of the two potential answers."

    nodes + """
    def remEdge(g) = match g | [a,>,b] => become [a,b]
    a = [na,>,nb] |&| [nc,>,nd] >> $remEdge
    b = [na,nb] |&| [nc,nd]
    """,["b"],
    "Collection enpiped to dollar applied transform. Ensure size of output collecition is same as input collection"

    nodes + """
    def remEdge(g) = match g | [a,>,b] => become [a,b]
    a = [na,>,nb,>,nd] |&| [ne,>,nf,>,na] >> $remEdge
    b = [na,nb,>,nd] |&| [ne,nf,>,na]
    c = [na,nb,>,nd] |&| [ne,>,nf,na]
    d = [na,>,nb,nd] |&| [ne,>,nf,na]  
    e = [na,>,nb,nd] |&| [ne,nf,>,na]
    """,["b";"c";"d";"e"],
    "Collection enpiped to dollar applied transform. Each graph in collection has multiple matches. "
    + "Ensure transform only picks one match in each graph in collection"
]

let maybeTransforms = [
    nodes + """
    def edge(g) = match g | [a,>,b] => become [a,>,b]
    a = [na,nb,nc] >> edge?
    b = [na,nb,nc]
    ""","Enpiping graph into a maybe applied transform that will not match. Ensure original graph is returned."

    nodes + """
    def doubleEdge(g) = match g | [a,>,b] => become [a,<>,b]
    a = [na,>,nb,nc] >> doubleEdge?
    b = [na,<>,nb,nc]
    ""","Enpiping graph into a maybe applied transform that will match. "
    + "Ensure normal application means correct graph is returned."

    nodes + """
    def weightEdge(g,x,y) =
        w = x+y
        match g
        | [a,>,b] => become [a,w>,b]
    a = ([na,nb,20>,nc],200,300) >> weightEdge?
    b = [na,nb,20>,nc]
    ""","Enpiping graph into a 3-argument transform that will not match. Ensure only the first param is returned."

    nodes + """
    def weightEdge(g,x,y) =
        w = x+y
        match g
        | [a,>,b] => become [a,w>,b]
    a = ([na,nb,>,nc],200,300) >> weightEdge?
    b = [na,nb,500>,nc]
    ""","Enpiping graph into a 3-argument transform that will match. "
    + "Ensure normal application means correct graph is returned."

    nodes + """
    def addEdge(g) = match g | [x,y] where &x == "theNode" => become [x,>,y]
    def doubleEdge(g) = match g | [x,>,y] => become [x,<>,y]
    node = "theNode"::10
    g = [node,na]
    a = g >> addEdge? |> doubleEdge?
    b = [node,<>,na]
    """, "Pipeline of two maybe applied transforms, both match. "
    + "Ensure normal application is used."

    nodes + """
    def removeEdgePair(g) = match g | [x,>,y,>,z] => become [x,y,z]
    def doubleEdge(g) = match g | [x,>,y] and not [x,<,y] => become [x,<>,y]
    g = [na,<>,nb]
    a = g >> removeEdgePair? |> doubleEdge?
    b = [na,<>,nb]
    """, "Pipeline of two maybe applied transforms, neither match. Ensure input of first is retruned."

    nodes + """
    def removeDoubleEdge(g) = match g | [x,<>,y] => become [x,y]
    def doubleEdge(g) = match g | [x,>,y] and not [x,<,y] => become [x,<>,y]
    g = [na,>,nb]
    a = g >> removeDoubleEdge? |> doubleEdge?
    b = [na,<>,nb]
    """, "Pipeline of two maybe applied transforms, first doesn't match, second does."
    + "Ensure input of first is applied to second."


    nodes + """
    def removeEdge(g) = match g | [x,>,y] => become [x,y]
    def doubleEdge(g) = match g | [x,>,y] and not [x,<,y] => become [x,<>,y]
    g = [na,>,nb]
    a = g >> removeEdge? |> doubleEdge?
    b = [na,nb]
    """, "Pipeline of two maybe applied transforms, second doesn't match. Ensure output of first is retruned."

    nodes + """
    def removeEdge(g) = match g | [x,<>,y,>,z] => become [x,>,y]
    def doubleEdge(g) = match g | [x,>,y] => become [x,<>,y]
    g = [na,<>,nb,>,nc]
    a = g >> removeEdge? |> doubleEdge?
    b = [na,<>,nb]
    """, "Pipeline of two maybe applied transforms, both match. Ensure normal application is used"

    nodes + """
    def removeEdge(g) = match g | [x,>,y] => become [x,y]
    def doubleEdge(g) = match g | [x,>,y] and not [x,<,y] => become [x,<>,y]
    g = [na,>,nb]
    a = g >> (removeEdge |> doubleEdge)?
    b = [na,>,nb]
    """, "Pipeline of two transforms all maybe applied, second doesn't match. Ensure input of first is retruned."
    
    nodes + """
    def removeEdge(g,p1,p2) = match g | [x,>,y] => become [x,y]
    def doubleEdge(g,p1,p2) = match g | [x,>,y] => become [x,<>,y]
    g = [na,>,nb]
    a = (g,10,20,30,40) >> (removeEdge |> doubleEdge)?
    b = [na,>,nb]
    """, "Pipeline of two transforms all maybe applied, second doesn't match, first is 3-argument. "
    + "Ensure first argument of input is retruned."
]

let mixingModifiers = [
    nodes + """
    def remEdge(g) = match g | [a,>,b] => become [a,b]
    a = [na,>,nb,>,nc,>,nd] >> $remEdge ** 2
    b = [na,>,nb,nc,nd]
    c = [na,nb,>,nc,nd]
    d = [na,nb,nc,>,nd]
    """,["b";"c";"d"],
    "Mulaplied dollar applied transform. Ensure answer is only one of 3 potential answers. "
    + "Also ensure $ has higher precedence than **."

    nodes + """
    def remEdge(g) = match g | [a,>,b] => become [a,b]
    a = [na,>,nb,>,nc,>,nd] >> $remEdge!
    b = [na,nb,nc,nd]
    """, ["b"],
    "ALAP and dollar applied transform. Ensure output is a single graph and $ has higher precedence than !."
]

[<Tests>]
let test =
    // These tests contain multiple applications of functions
    let name = "Mulaplied functions"
    testList name (multipleApplicationFunctions |> abTwinVarTests)

[<Tests>]
let test2 =
    // These tests contain multiple applications of transforms
    let name = "Mulaplied transforms"
    testList name (multipleApplicationTransforms |> abTwinVarTests)

[<Tests>]
let test3 =
    // These tests contain ALAP applications of transforms
    let name = "ALAP application of transforms"
    testList name (alapTransforms |> abTwinVarTests)

[<Tests>]
let test4 =
    // These tests contain dollar applications of transforms
    let name = "Dollar application of transforms"
    testList name (dollarTransforms |> aOneOfTests)

[<Tests>]
let test5 =
    // These tests contain maybe applications of transforms
    let name = "Maybe application of transforms"
    testList name (maybeTransforms |> abTwinVarTests)

[<Tests>]
let test6 =
    // These tests contain transforms applied with multiple modifiers
    let name = "Dollar application of transforms"
    testList name (mixingModifiers |> aOneOfTests)
    