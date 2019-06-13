module Bilbo.Tests.EvaluatorTests.AssignmentExpression

open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

let fieldAssignTests = [
    """
    type City = capital,population
    got = City(True,100)
    got.capital = False
    exp = City(False,100)
    ""","Single field assignment within object"

    """
    type City = capital,population
    got = City(True,100)
    got.capital = False
    got.population = 30
    exp = City(False,30)
    """, "Field assignment twice within same object"
]

let nodeLoadAssignTests = [
    """
    type City = capital,population
    got = "London"::City(True,100)
    #got = City(True,70)
    exp = "London"::City(True,70)
    """, "Reassignment of node load"

    """
    type City = capital,population
    got = "London"::City(True,100)
    #got.capital = False
    exp = "London"::City(False,100)
    """, "Reassignment of node load field"

    """
    type City = capital,loc
    type Location = x,y
    got = "London"::City(True,Location(200,500))
    #got.loc.y = 800
    exp = "London"::City(True,Location(200,800))
    """, "Reassignment of node load field where field is itself an object"
]

let nodeIdAssignTests = [
    """
    type City = capital,population
    got = "London"::City(True,100)
    &got = "London City"
    exp = "London City"::City(True,100)
    """, "Reassignment of node id"

    """
    type City = capital,population
    type Place = name, country
    got = Place("London", "UK")::City(True,100)
    &got.name = "London City"
    exp = Place("London City", "UK")::City(True,100)
    """, "Reassignment of node id field"

    """
    type City = capital,population
    type Place = name, location
    type Location = x,y
    loc = Location(400,800)
    got = Place("London", loc)::City(True,100)
    &got.location.y = 600
    exp = Place("London", Location(400,600))::City(True,100)
    """, "Reassignment of node id field where field is itself an object"
]

let nodeLoadFieldAssignTests = [
    """
    type City = capital,population
    got = "London"::City(True,100)
    got..population = 70
    exp = "London"::City(True,70)
    """, "Reassignment of node load"

    """
    type City = capital,loc
    type Location = x,y
    got = "London"::City(True,Location(200,500))
    got..loc.y = 800
    exp = "London"::City(True,Location(200,800))
    """, "Reassignment of node load field where field is itself an object"
]

[<Tests>]
let test =
    let name = "Field assignment on LHS"
    testList name (fieldAssignTests |> gotExpTwinVarTests)

[<Tests>]
let test2 =
    let name = "Node load assignment on LHS"
    testList name (nodeLoadAssignTests |> gotExpTwinVarTests)
    
[<Tests>]
let test3 =
    let name = "Node id assignment on LHS"
    testList name (nodeIdAssignTests |> gotExpTwinVarTests)

[<Tests>]
let test4 =
    let name = "Node load field assignment (using ..) on LHS"
    testList name (nodeLoadFieldAssignTests |> gotExpTwinVarTests)