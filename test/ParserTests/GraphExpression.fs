module Bilbo.Tests.ParserTests.GraphExpression

open Expecto
open Bilbo.Parser.Ast
open Bilbo.Tests.ParserTests.Helpers

let NDv x = x |> VAR |> ND
let NDi n = n |> INT |> SExpr |> ND

// Some of these tests are semantically invalid since graphs cannot take (non-node) objects 
let nodeOnlyPaths = [
    "a=[]", "a",  [] |> PATH, "Empty path"
    "a'=[b']", "a'",  ["b'"] |> List.map NDv |> PATH, "Node only, 1 var path"
    "a=[b,c,d]", "a", ["b";"c";"d"] |> List.map NDv |> PATH, "Node only, 3 var path";
    "a = [10,11,12,13]", "a", [10;11;12;13] |> List.map NDi |> PATH, "Int literal path"
    ]

let b = ([LEDGE (VAR "f") (None) (VAR "g")], (List.map NDv  ["g";"h";"i"])) ||> List.append |> PATH

let singleEdgePaths = [
    "a=[b,>,c]", "a", [REDGE (VAR "b") (None) (VAR "c")] |> PATH, "Unweighted single right edge";
    "a=[b,<,c]", "a", [LEDGE (VAR "b") (None) (VAR "c")] |> PATH, "Unweighted single left edge";
    "a=[b,<>,c]", "a", [BIEDGE (VAR "b") (None) (VAR "c")] |> PATH, "Unweighted single bidirectional edge";
    "a=[b,c,>,d,e]", "a",
        [NDv "b"; REDGE (VAR "c") (None) (VAR "d"); NDv "e"] |> PATH,
        "Unweighted single right edge, other nodes";
    "a=[f,<,g,h,i,j]", "a",
        ([LEDGE (VAR "f") (None) (VAR "g")], (List.map NDv  ["h";"i";"j"])) ||> List.append |> PATH,
        "Unweighted single left edge, other nodes";
]

[<Tests>]
let tests =
    testList "Node only path tests" (exprTests nodeOnlyPaths)

[<Tests>]
let tests2 =
    testList "Single edge path tests" (exprTests singleEdgePaths)