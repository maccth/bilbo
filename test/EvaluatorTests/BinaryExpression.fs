module Bilbo.Tests.EvaluatorTests.BinaryExpression

open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

let graphBinOps = [
    nodes + """
    a = [na,>,nb] - [na]
    b = [nb]
    """, "Graph - node. Ensure removal of edges that have the node as an endpoint."
    
    nodes + """
    a = [na,>,nb] - [na,>,nb]
    b = [na,nb]
    """, "Graph - edge. Ensure only endpoint nodes are removed."
]

let graphCollectionBinOps = [
    nodes + """
    g = [na,nb]
    c = [na,nc] |&| [nb,nd]
    a = g + c
    b = [na,nb,nc] |&| [na,nb,nd]
    ""","Graph + collection. Nodes only. Ensure previous nodes are kept and new nodes are added."

    nodes + """
    g = [na,nb]
    c = [na,nc] |&| [nb,nd]
    a = c + g
    b = [na,nb,nc] |&| [na,nb,nd]
    ""","Collection + graph. Nodes only. Ensure previous nodes are kept and new nodes are added."

    nodes + """
    g = [na,1>,nb,ne]
    c = [na,2>,nc] |&| [nb,3>,nd]
    a = g + c
    b = [na,1>,nb] + [na,2>,nc] + [ne] |&| [na,1>,nb,3>,nd] + [ne]
    ""","Graph + collection. Nodes and edges. Ensure previous elements are kept and new ones are added."

    nodes + """
    g = [na,1>,nb,ne]
    c = [na,2>,nc] |&| [nb,3>,nd]
    a = c + g
    b = [na,1>,nb] + [na,2>,nc] + [ne] |&| [na,1>,nb,3>,nd] + [ne]
    ""","Collection + graph. Nodes and edges. Ensure previous elements are kept and new ones are added."

    nodes + """
    nA = "a"::"First"
    nA2 = "a"::"Second"
    nB = "b"::"First"
    nB2 = "b"::"Second"

    g = [nA,1>,nB,ne]
    c = [nA2,2>,nc] |&| [nB2,3>,nd]
    a = g + c
    b = [nA2,1>,nB] + [nA2,2>,nc] + [ne] |&| [nA,1>,nB2,3>,nd] + [ne]
    ""","Graph + collection. Nodes and edges with node id clash. Ensure latest ('right most') load is kept."

    nodes + """
    nA = "a"::"First"
    nA2 = "a"::"Second"
    nB = "b"::"First"
    nB2 = "b"::"Second"

    g = [nA,1>,nB,ne]
    c = [nA2,2>,nc] |&| [nB2,3>,nd]
    a = c + g
    b = [nA,1>,nB] + [nA,2>,nc] + [ne] |&| [nA,1>,nB,3>,nd] + [ne]
    ""","Collection + graph. Nodes and edges with node id clash. Ensure latest ('right most') load is kept."

    nodes + """
    g = [na,nb]
    c = [na,nc] |&| [nb,nd]
    a = g - c
    b = [nb] |&| [na]
    ""","Graph - collection. Nodes only. Ensure matching nodes are removed."

    nodes + """
    g = [na,nb]
    c = [na,nc] |&| [nb,nd]
    a = c - g
    b = [nc] |&| [nd]
    ""","Collection - graph. Nodes only. Ensure matching nodes are removed."
]

[<Tests>]
let test =
    let name = "Binary operations on graphs"
    testList name (graphBinOps |> abTwinVarTests)

[<Tests>]
let test2 =
    let name = "Binary operations on graphs and collections"
    testList name (graphCollectionBinOps |> abTwinVarTests)

   