module Bilbo.Tests.EvaluatorTests.Expression

open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

// Property based testing for evaluation
// =====================================
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

let primTypesSingleVarAssign = [
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
    "\"!@£$%^&*()~{}:|<>\"", "!@£$%^&*()~{}:|<>" |> String |> Value, "String with symbols"
]

let primTypesSingleVarBinOp = [
    "345^89", 345.0**89.0 |> Float |> Value, "Int int power, both pos" 
    "7^-3", 7.0**(-3.0) |> Float |> Value, "Int int power, pos base, neg power" 
    "-45^3", (-45.0)**(3.0) |> Float |> Value, "Int int power, neg base, pos power" 
    "-23^-4", (-23.0)**(-4.0) |> Float |> Value, "Int int power, both neg, even power" 
    "-17^-5", (-17.0)**(-5.0) |> Float |> Value, "Int int power, both neg, odd power"
    "0^0", 1.0 |> Float |> Value, "Int int power, zeros"
    "1^0", 1.0 |> Float |> Value, "Int int power, 1**0"
    "1^1", 1.0 |> Float |> Value, "Int int power, 1**1"

    "730*32123", 730*32123 |> Int |> Value, "Int int multiplication, both pos"
    "-263*3236", -263*3236 |> Int |> Value, "Int int multiplication, neg lhs"
    "1287*(-313)", 1287*(-313) |> Int |> Value, "Int int multiplication, neg rhs"
    "(-5234)*(-3873)", (-5234)*(-3873) |> Int |> Value, "Int int multiplication, both neg"
    "198261*1", 198261 |> Int |> Value, "Int int multiplcation, both pos, 1 rhs" 
    "1*1232", 1232 |> Int |> Value, "Int int multiplcation, both pos, 1 lhs" 
    "-1*823", -823 |> Int |> Value, "Int int multiplcation, -1 lhs" 
    "1245*-1", -1245 |> Int |> Value, "Int int multiplcation, -1 rhs" 
    "198261*0", 0 |> Int |> Value, "Int int multiplcation, 0 rhs" 
    "0*1232", 0 |> Int |> Value, "Int int multiplcation, 0 lhs" 
    "-0*823", 0 |> Int |> Value, "Int int multiplcation, -0 lhs" 
    "1245*-0", 0 |> Int |> Value, "Int int multiplcation, -0 rhs" 

    "1239764/5232", 1239764/5232 |> Int |> Value, "Int int division, both pos"
    "-734/32", -734/32 |> Int |> Value, "Int int division, neg numerator"
    "862454/-413", 862454/(-413) |> Int |> Value, "Int int division, neg denominator"
    "-6834/-723", (-6834)/(-723) |> Int |> Value, "Int int division, both neg"
    "0/9816", 0 |> Int |> Value, "Int int division, zero numerator" 
    "-97134/1", -97134 |> Int |> Value, "Int int division, one denominator" 


    "86234%2312", 86234%2312 |> Int |> Value, "Int int modulo, both pos"
    "-87534%934", (-87534)%934 |> Int |> Value, "Int int modulo, negative numerator"
    "529225%-97263", 529225%(-97263) |> Int |> Value, "Int int modulo, negative denominator"
    "-7513%-875", (-7513)%(-875) |> Int |> Value, "Int int modulo, both negative"
    "18753871%1", 18753871%1 |> Int |> Value, "Int int modulo, denominator one"
    "0%18753871", 0 |> Int |> Value, "Int int modulo, numerator zero"

    "8969234+126854", 8969234+126854 |> Int |> Value, "Int int addition, both pos"
    "-087634+69234", -087634+69234 |> Int |> Value, "Int int addition, neg lhs"
    "520673+-72962", 520673+(-72962) |> Int |> Value, "Int int addition, neg rhs"
    "(-332863)+(-15235)", (-332863)+(-15235) |> Int |> Value, "Int int addition, both neg"
    "12086312+0", 12086312 |> Int |> Value, "Int int addition, adding zero"
    "-925643+(-0)", -925643 |> Int |> Value, "Int int addition, adding minus zero"

    "8969234-126854", 8969234-126854 |> Int |> Value, "Int int subtraction, both pos"
    "-087634-69234", -087634-69234 |> Int |> Value, "Int int subtraction, neg lhs"
    "520673--72962", 520673-(-72962) |> Int |> Value, "Int int subtraction, neg rhs"
    "(-332863)-(-15235)", (-332863)-(-15235) |> Int |> Value, "Int int subtraction, both neg"
    "12086312-0", 12086312 |> Int |> Value, "Int int subtraction, subbing zero"
    "-925643-(-0)", -925643 |> Int |> Value, "Int int subtraction, subbing minus zero"
]

let quickSingleVarAssignTest strMean mean des =
    let var = "a"
    let codeStr = var + " = " + strMean
    codeStr, var, mean, des

let quickSingleVarAssignTests testDatalst =
    List.map (fun (s,m,d) -> quickSingleVarAssignTest s m d) testDatalst

[<Tests>]
let tests =
    let name = "primative type literals single var assign"
    testList name (primTypesSingleVarAssign |> quickSingleVarAssignTests |> singleVarTests)

[<Tests>]
let tests2 =
    let name = "primative type single var binary operations"
    testList name (primTypesSingleVarBinOp |> quickSingleVarAssignTests |> singleVarTests)