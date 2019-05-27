module Bilbo.Tests.EvaluatorTests.Value

open Bilbo.Common.Value
open Bilbo.Tests.EvaluatorTests.Helpers
open Expecto

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

let primTypesSingleVarArithmeticOps = [
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

let primTypesComparisonOperators = [
    "0.0<0", false |> Bool |> Value, "Less then, zeros"
    "(-0)<0.0", false |> Bool |> Value, "Less than, minus zero lhs"
    "0<(-0)", false |> Bool |> Value, "Less than, minus zero rhs"
    "10<40.0", true |> Bool |> Value, "Less than, true, both pos"
    "19.0<18", false |> Bool |> Value, "Less than, false, both pos"
    "(-20.0)<(-15)", true |> Bool |> Value, "Less than, True, both neg"
    "(-3.0)<(10.0)", true |> Bool |> Value, "Less than, True, lhs neg"
    "6<(-1.0)", false |> Bool |> Value, "Less than, False, rhs neg"
    "(-6.0)<(-10)", false |> Bool |> Value, "Less than, False, both neg neg"
    "981263.0<6798203", true |> Bool |> Value, "Less than, true, big nums"

    "0<=0", true |> Bool |> Value, "Less than equal, zeros" 
    "0.0<=0", true |> Bool |> Value, "Less than equal, zeros, int float mix"
    "-0<=-0", true |> Bool |> Value, "Less than equal, neg zeros" 
    "-10.0<=-10", true |> Bool |> Value, "Less than equal, true, neg 10s"
    "10<=10", true |> Bool |> Value, "Less than equal, true, 10s"
    "0.1<=0", false |> Bool |> Value, "Less than equal, false, int float mix"
    "10<=11", true |> Bool |> Value, "Less than equal, true"
    "10<=9", false |> Bool |> Value, "Less than equal, false,"
    "128371<=8563", false |> Bool |> Value, "Less than equal, false, both pos big nums"
    "128371<=-8563", false |> Bool |> Value, "Less than equal, false, rhs neg"
    "-128371<=8563", true |> Bool |> Value, "Less than equal, true, lhs neg"
    "-128371<=-8563", true |> Bool |> Value, "Less than equal, true, both neg"

    "0.0>0", false |> Bool |> Value, "Greater then, zeros"
    "(-0)>0.0", false |> Bool |> Value, "Greater than, minus zero lhs"
    "0>(-0)", false |> Bool |> Value, "Greater than, minus zero rhs"
    "10>40.0", false |> Bool |> Value, "Greater than, false, both pos"
    "19.0>18", true |> Bool |> Value, "Greater than, true, both pos"
    "(-20.0)>(-15)", false |> Bool |> Value, "Greater than, false, both neg"
    "(-3.0)>(10.0)", false |> Bool |> Value, "Greater than, false, lhs neg"
    "6>(-1.0)", true |> Bool |> Value, "Greater than, true, rhs neg"
    "(-6.0)>(-10)", true |> Bool |> Value, "Greater than, true, both neg neg"
    "981263.0>6798203", false |> Bool |> Value, "Greater than, false, big nums"

    "0>=0", true |> Bool |> Value, "Greater than equal, zeros" 
    "0.0>=0", true |> Bool |> Value, "Greater than equal, zeros, int float mix"
    "-0>=-0", true |> Bool |> Value, "Greater than equal, neg zeros" 
    "-10.0>=-10", true |> Bool |> Value, "Greater than equal, true, neg 10s"
    "10>=10", true |> Bool |> Value, "Greater than equal, true, 10s"
    "0.1>=0", true |> Bool |> Value, "Greater than equal, true, int float mix"
    "10>=11", false |> Bool |> Value, "Greater than equal, false"
    "10>=9", true |> Bool |> Value, "Greater than equal, true,"
    "128371>=8563", true |> Bool |> Value, "Greater than equal, true, both pos big nums"
    "128371>=-8563", true |> Bool |> Value, "Greater than equal, true, rhs neg"
    "-128371>=8563", false |> Bool |> Value, "Greater than equal, false, lhs neg"
    "-128371>=-8563", false |> Bool |> Value, "Greater than equal, false, both neg"
]

let primTypesLogicalOperators = [
    "True and True", true |> Bool |> Value, "Bool and, trues"
    "True and False", false |> Bool |> Value, "Bool and, mixed 1"
    "False and True", false |> Bool |> Value, "Bool and, mixed 2"
    "False and False", false |> Bool |> Value, "Bool and, falses"
    "True and True and False", false |> Bool |> Value, "Bool and 3"
    
    "True or True", true |> Bool |> Value, "Bool or, trues"
    "True or False", true |> Bool |> Value, "Bool or, mixed 1"
    "False or True", true |> Bool |> Value, "Bool or, mixed 2"
    "False or False", false |> Bool |> Value, "Bool or, falses"
    "False or False or True", true |> Bool |> Value, "Bool or 3"

    "True xor True", false |> Bool |> Value, "Bool xor, trues"
    "True xor False", true |> Bool |> Value, "Bool xor, mixed 1"
    "False xor True", true |> Bool |> Value, "Bool xor, mixed 2"
    "False xor False", false |> Bool |> Value, "Bool xor, falses"
    "True xor True xor True", true |> Bool |> Value, "Bool xor, 3s"


    "True and False or True", true |> Bool |> Value, "Bool mixed 3s, 1"
    "(True and False) or True", true |> Bool |> Value, "Bool mixed 3s, 2"
    "True and (True or False)", true |> Bool |> Value, "Bool mixed 3s, 3"
    "False or False xor True and True", true |> Bool |> Value, "Bool mixed 3s, 4"
    "((False or False) xor True) and True", true |> Bool |> Value, "Bool mixed 3s, 5"

    "0 and True", false |> Bool |> Value, "Int false"
    "1 and True", true |> Bool |> Value, "Int true"
    "-1 and True", true |> Bool |> Value, "Int true negative"
    "0.0 and True", false |> Bool |> Value, "Float false"
    "0.1 and True", true |> Bool |> Value, "Float true"
    "-0.1 and True", true |> Bool |> Value, "Float true negative"
    "\"\" and True", false |> Bool |> Value, "String false"
    "\"hi\" and True", true |> Bool |> Value, "String true"
    "\"False\" and True", true |> Bool |> Value, "String true with false quoted"

    "not True", false |> Bool |> Value, "Noting true"
    "not False", true |> Bool |> Value, "Noting false"
    "not 0", true |> Bool |> Value, "Noting int 0"
    "not 1", false |> Bool |> Value, "Noting small positive int"
    "not -1", false |> Bool |> Value, "Noting small negative int"
    "not 20", false |> Bool |> Value, "Noting positive int"
    "not -20", false |> Bool |> Value, "Noting negative int"
    "not 0.0", true |> Bool |> Value, "Noting float 0"
    "not 0.1", false |> Bool |> Value, "Noting small positive float"
    "not -0.1", false |> Bool |> Value, "Noting small negative float"
    "not 20", false |> Bool |> Value, "Noting positive float"
    "not -20", false |> Bool |> Value, "Noting negative float"
    "not \"\"", true |> Bool |> Value, "Noting empty string"
    "not \"False\"", false |> Bool |> Value, "Noting false string"
    "not \"True\"", false |> Bool |> Value, "Noting true string"
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
    let name = "primative type single var arithemtic operations"
    testList name (primTypesSingleVarArithmeticOps |> quickSingleVarAssignTests |> singleVarTests)

[<Tests>]
let tests3 =
    let name = "primative type single var arithemtic operations"
    testList name (primTypesComparisonOperators |> quickSingleVarAssignTests |> singleVarTests)

[<Tests>]
let tests4 =
    let name = "primative type single var arithemtic operations"
    testList name (primTypesLogicalOperators |> quickSingleVarAssignTests |> singleVarTests)