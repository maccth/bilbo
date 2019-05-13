module Bilbo.Tests.GraphTests.Helpers

open Bilbo.Common.Value

let bilboStr = String >> Value
let bilboInt = Int >> Value
let bilbo1 = 1 |> bilboInt
let ndSi id load = {id=id|>bilboStr; load=load|>bilboInt}
let nd1 id = ndSi id 1
let (.>.) nL nR = {source=nL; target=nR; weight=None}

let (|->) l r = Result.bind (r) l
let (|-->) l r =
    l |-> r
    |> function
    | Ok res -> res
    | Error e -> failwithf "Pipe result unpacker failed"