module Bilbo.Tests.EvaluatorTests.Expression

open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

// Property based for evaluation
// =============================
// let consAssignCodeStr (mean : 'T) (meanStr : string) (typ : 'T->Value) =
//     let codeStr = "a = " + meanStr
//     let mean = mean |> typ |> Value
//     runSingleVarTest codeStr "a" mean

// let intAssignTest = testProperty "Assignment is fun" <| 
//     fun (v : int) -> consAssignCodeStr v (string v) Int

// let boolAssignTest = testProperty "Assignment is fun4" <| 
//     fun (v : bool) ->
//         match v with
//         | true -> consAssignCodeStr v "True" Bool
//         | false -> consAssignCodeStr v "False" Bool
// let config = { FsCheckConfig.defaultConfig with maxTest = 10000 }
// let primTypesAssignTests = [intAssignTest; boolAssignTest]

let primativeTypesSingleVarLiteralAssign = [
    "10", 10 |> Int |> Value, "Positive int"
    "-10", -10 |> Int |> Value, "Negative int"
    "0", 0 |> Int |> Value, "Int zero"
    "-0", 0 |> Int |> Value, "Int negative zero"
    "2781534", 2781534 |> Int |> Value, "Big pos int"
    "-9862352", -9862352 |> Int |> Value, "Big neg int"

    "True", true |> Bool |> Value, "Bool true"
    "False", false |> Bool |> Value, "Bool false"

    "56.4", 56.4 |> Float |> Value, "Positive float"
    "-45.2", -45.2 |> Float |> Value, "Negative float"
    "0.0", 0.0 |> Float |> Value, "Float zero"
    "-0.0", 0.0 |> Float |> Value, "Float negative zero"
    "17623.97823", 17623.97823 |> Float |> Value, "Big pos float" 
    "-8429.02894", -8429.02894 |> Float |> Value, "Big neg float"

    "\"Hello world!\"", "Hello world!" |> String |> Value, "Basic string"
    "\"    goodbye world!  \"", "    goodbye world!  " |> String |> Value, "String with spaces"
    "\"!@£$%^&*()~{}:|<>\"", "!@£$%^&*()~{}:|<>" |> String |> Value, "String with with symbols"
]

let quickSingleVarAssignTest strMean mean des =
    let var = "a"
    let codeStr = var + " = " + strMean
    codeStr, var, mean, des

let quickSingleVarAssignTests testDatalst =
    List.map (fun (s,m,d) -> quickSingleVarAssignTest s m d) testDatalst

[<Tests>]
let tests =
    testList "basic tests" (primativeTypesSingleVarLiteralAssign |> quickSingleVarAssignTests |> singleVarTests)
