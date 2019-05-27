module Bilbo.Tests.EvaluatorTests.AssignmentExpression

open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

let fieldAssignTests = [
    """
    type City = capital,population
    a = City(True,100)
    a.capital = False
    b = City(False,100)
    ""","Single field assignment within object"

    """
    type City = capital,population
    a = City(True,100)
    a.capital = False
    a.population = 30
    b = City(False,30)
    """, "Field assignment twice within same object"
]

let nodeLoadAssignTests = [
    """
    type City = capital,population
    a = "London"::City(True,100)
    #a = City(True,70)
    b = "London"::City(True,70)
    """, "Reassignment of node load"

    """
    type City = capital,population
    a = "London"::City(True,100)
    #a.capital = False
    b = "London"::City(False,100)
    """, "Reassignment of node load field"

    """
    type City = capital,loc
    type Location = x,y
    a = "London"::City(True,Location(200,500))
    #a.loc.y = 800
    b = "London"::City(True,Location(200,800))
    """, "Reassignment of node load field where field is itself an object"
]

let nodeIdAssignTests = [
    """
    type City = capital,population
    a = "London"::City(True,100)
    &a = "London City"
    b = "London City"::City(True,100)
    """, "Reassignment of node id"

    """
    type City = capital,population
    type Place = name, country
    a = Place("London", "UK")::City(True,100)
    &a.name = "London City"
    b = Place("London City", "UK")::City(True,100)
    """, "Reassignment of node id field"

    """
    type City = capital,population
    type Place = name, location
    type Location = x,y
    loc = Location(400,800)
    a = Place("London", loc)::City(True,100)
    &a.location.y = 600
    b = Place("London", Location(400,600))::City(True,100)
    """, "Reassignment of node id field where field is itself an object"
]

let nodeLoadFieldAssignTests = [
    """
    type City = capital,population
    a = "London"::City(True,100)
    a->population = 70
    b = "London"::City(True,70)
    """, "Reassignment of node load"

    """
    type City = capital,loc
    type Location = x,y
    a = "London"::City(True,Location(200,500))
    a->loc.y = 800
    b = "London"::City(True,Location(200,800))
    """, "Reassignment of node load field where field is itself an object"
]

[<Tests>]
let test =
    let name = "Field assignment on LHS"
    testList name (fieldAssignTests |> abTwinVarTests)

[<Tests>]
let test2 =
    let name = "Node load assignment on LHS"
    testList name (nodeLoadAssignTests |> abTwinVarTests)
    
[<Tests>]
let test3 =
    let name = "Node id assignment on LHS"
    testList name (nodeIdAssignTests |> abTwinVarTests)

[<Tests>]
let test4 =
    let name = "Node load field assignment (using ->) on LHS"
    testList name (nodeLoadFieldAssignTests |> abTwinVarTests)