module TestParser

open Expecto
open Tests

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv |> ignore
    0
