module Bilbo.Tests.ParserTests.GraphExpression

open Expecto
open Bilbo.Common.Ast
open Bilbo.Tests.ParserTests.Helpers

let NDv x = x |> VAR |> ND
let NDi n = n |> INT |> SExpr |> ND
let W x = x |> SExpr |> Some

// Some of these tests are semantically invalid since graphs cannot take (non-node) objects 
let nodeOnlyPaths = [
    "a=[]", "a",  [] |> PATH, "Empty path"
    "a'=[b']", "a'",  ["b'"] |> List.map NDv |> PATH, "Node only, 1 var path"
    "a=[b,c,d]", "a", ["b";"c";"d"] |> List.map NDv |> PATH, "Node only, 3 var path";
    "a = [10,11,12,13]", "a", [10;11;12;13] |> List.map NDi |> PATH, "Int literal path"
    ]

let singleEdgePaths = [
    "a=[b,>,c]",
        "a",
        [REDGE (VAR "b") (None) (VAR "c")] |> PATH,
        "Unweighted single right edge";
    "a=[b,<,c]",
        "a",
        [LEDGE (VAR "b") (None) (VAR "c")] |> PATH,
        "Unweighted single left edge";
    "a=[b,<>,c]",
        "a",
        [BIEDGE (VAR "b") (None) (VAR "c")] |> PATH,
        "Unweighted single bidirectional edge";
    "a=[b,c,>,d,e]",
        "a",
        [NDv "b"; REDGE (VAR "c") (None) (VAR "d"); NDv "e"] |> PATH,
        "Unweighted single right edge, other nodes";
    "a=[f,<,g,h,i,j]",
        "a",
        ([LEDGE (VAR "f") (None) (VAR "g")],
            (List.map NDv  ["h";"i";"j"])) ||> List.append |> PATH,
        "Unweighted single left edge, other nodes";

    "a=[b,10>,c]",
        "a",
        [REDGE (VAR "b") (10 |> INT |> W) (VAR "c")] |> PATH,
        "Weighted single right edge";
    "a=[b,<-14.5,c]",
        "a",
        [LEDGE (VAR "b") (-14.5 |> FLT |> W) (VAR "c")] |> PATH,
        "Weighted single left edge";
    "a=[b,<False>,c]",
        "a",
        [BIEDGE (VAR "b") (false |> BOOL |> W) (VAR "c")] |> PATH,
        "Weighted single bidirectional edge";
    "a=[b,c,\"string weight\">,d,e]",
        "a",
        [NDv "b"; REDGE (VAR "c") ("string weight" |> STR |> W) (VAR "d"); NDv "e"] |> PATH,
        "Weighted single right edge, other nodes";
    "a=[f,<a,g,h,i,j]",
        "a",
        ([LEDGE (VAR "f") ("a" |> VAR |> Some) (VAR "g")],
            (List.map NDv  ["h";"i";"j"])) ||> List.append |> PATH,
        "Weighted single left edge, other nodes";
]

let multiEdgePaths =[
    "a=[b,c>,d,<e>,f,<f,g]",
        "a",
        [
            REDGE (VAR "b") (Some (VAR "c")) (VAR "d");
            BIEDGE (VAR "d") (Some (VAR "e")) (VAR "f");
            LEDGE (VAR "f") (Some (VAR "f")) (VAR "g");
        ] |> PATH,
        "Variable weighted path, multiple edges";
]

[<Tests>]
let tests =
    testList "Node only path tests" (exprTests nodeOnlyPaths)

[<Tests>]
let tests2 =
    testList "Single edge path tests" (exprTests singleEdgePaths)

[<Tests>]
let tests3 =
    testList "Single edge path tests" (exprTests multiEdgePaths)
    