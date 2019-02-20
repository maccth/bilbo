module Tests.ParserTests.Main
open Expecto
open Sample
// open Bilbo.Parser.Main

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv
