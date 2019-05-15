module Bilbo.Tests.GraphTests.Helpers

open Bilbo.Common.Extensions
open Bilbo.Common.Value

let bStr = String >> Value
let bInt = Int >> Value
let b1 = 1 |> bInt
let ndSi id load = {id=id|>bStr; load=load|>bInt}
let nd1 id = ndSi id 1

let (.>.) nL nR = {Edge.source=nL; target=nR; weight=None}
let (.<.) nL nR = nR .>. nL
let (.<>.) nL nR = [nL .>. nR; nR .>. nL]

let (..>.) nL nR w = {Edge.source=nL; target=nR; weight=Some w}
let (.<..) nL nR w = {Edge.source=nR; target=nL; weight=Some w}

let (|-->) l r =
    l |-> r
    |> function
    | Ok res -> res
    | Error e -> failwithf "Pipe result unpacker failed"