module Bilbo.Tests.EvaluatorTests.Pipeline

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
    def incWeight(g) = match g | [a,x>,b] -> become [a,x+1>,b]
    a = [na,100>,nb] >> incWeight ** 1
    b = [na,101>,nb]
    """, "Single param transform mulaplied once"


    nodes + """
    def incWeight(g) = match g | [a,x>,b] -> become [a,x+1>,b]
    a = [na,100>,nb] >> incWeight ** 5
    b = [na,105>,nb]
    """, "Single param transform mulaplied five times"

    nodes + """
    def incWeight(g) = match g | [a,x>,b] -> become [a,x+1>,b]
    a = [na,100>,nb,0>,nc] >> incWeight ** 2
    b = [na,102>,nb,0>,nc]
        |&| [na,101>,nb,1>,nc]
        |&| [na,100>,nb,2>,nc]
    """, "Single param transform mulaplied twice but with multiple possible matches"
]

let alapTransforms = [
    nodes + """
    def join(g) = match g | [a,b] and not [a,>,b] -> become [a,>,b]
    a = [na,nb] >> join!
    b = [na,<>,nb]
    ""","Single param transform ALAP application, applied twice, with single graph output"
    
    nodes + """
    def break(g) = match g | [a,>,b] -> become [a,b]
    a = [na,>,nb,>,nc,>,nd] >> break!
    b = [na,nb,nc,nd]
    ""","Single param transform ALAP application, applied thrice, with single graph output"

    nodes + """
    def break(g) = match g | [a,>,b] -> become [a,b]
    a = [na,<>,nb] |&| [nc,<>,nd] >> break!
    b = [na,nb] |&| [nc,nd]
    ""","Single param transform ALAP application, applied to collection, with collection output"

    nodes + """
    def join(g) = match g | [a,b] and not [a,x>,b]-> become [a,0>,b]
    def incWeight(g) =
        match g 
        | [a,x>,b] where x==0 ->
            y = x+1
            become [a,y>,b]
    a = [na,nb] >> (join |> incWeight)!
    b = [na,<1>,nb]
    ""","Two single param transforms in a pipeline applied ALAP"

    nodes + """
    def join(g) = match g | [a,b] and not [a,x>,b]-> become [a,0>,b]
    def incWeight(g) =
        match g 
        | [a,x>,b] where x==0 ->
            y = x+1
            become [a,y>,b]
    a = [na,nb] >> join! |> incWeight!
    b = [na,<1>,nb]
    ""","Two single param transforms in a pipeline each applied ALAP"
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

    
    