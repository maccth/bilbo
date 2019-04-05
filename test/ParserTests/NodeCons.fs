module Bilbo.Tests.ParserTests.NodeCons

open Expecto
open Bilbo.Parser.Ast
open Bilbo.Tests.ParserTests.Helpers

let nodeConsTests = [
    "String identifier, int load", "a = \"NodeId\"::-10", "a", BE (STR  "NodeId") Dot (INT -10);
    "Int identifier, bool load", "a = 0::True", "a", BE (INT 0) NodeCons (BOOL true);
    "Var identifier, var load", "a = b::c", "a", (VAR "b", NodeCons, VAR "c") |> BinExpr;
]