module Bilbo.Tests.GraphTests.Helpers

open Bilbo.Common.Extensions
open Bilbo.Common.Value

let bStr = String >> Value
let bInt = Int >> Value
let b1 = 1 |> bInt
let ndSi id load = {id=id|>bStr; load=load|>bInt}
let nd1 id = ndSi id 1

let und id = {nid=id|>bStr}

let (.>.) nL nR = {Edge.source=nL; target=nR; weight=None}
let (.<.) nL nR = nR .>. nL
let (.<>.) nL nR = [nL .>. nR; nR .>. nL]
let (..>.) nL nR w = {Edge.source=nL; target=nR; weight=Some w}
let (.<..) nL nR w = {Edge.source=nR; target=nL; weight=Some w}

let (.?>.) (nL : UnboundNode) nR = {source=nL; target=nR; weight=None}
let (.<?.) (nL : UnboundNode) nR = nR .?>. nL
let (.<?>.) (nL : UnboundNode) nR = [nL .?>. nR; nR .?>. nL]
let (..?>.) (nL : UnboundNode) nR w = {source=nL; target=nR; weight=Some w}
let (.<?..) (nL : UnboundNode) nR w = {source=nR; target=nL; weight=Some w}

// Some ready-made nodes for use in the tests.
// We'll use upper case letters for host graph nodes and lower case for pattern graph nodes
let A = ndSi "A" 1
let B = ndSi "B" 2
let C = ndSi "C" 3
let D = ndSi "D" 4
let E = ndSi "E" 5

let a = ndSi "a" 10
let b = ndSi "b" 20
let c = ndSi "c" 30
let d = ndSi "d" 40
let e = ndSi "e" 50

// The unbound equivalent nodes for pattern graphs
let a' = und "a"
let b' = und "b"
let c' = und "c"
let d' = und "d"
let e' = und "e"

let (|-->) l r =
    l |-> r
    |> function
    | Ok res -> res
    | Error e -> failwithf "Pipe result unpacker failed"

let consNMapping m = m |> List.map (fun (k,v) -> Value (String k), Value (String v)) |> Map.ofList
let consNMappings mapLst = List.map consNMapping mapLst
let consEMapping m : Map<EdgeId,EdgeId> = m |> Map.ofList