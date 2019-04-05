module Bilbo.Tests.ParserTests.NodeCons

open Expecto
open Bilbo.Parser.Ast
open Bilbo.Tests.ParserTests.Helpers

// TODO: Move to new file since NodeCons is not an SExpr
let nodeConsTests = [
    "String identifier, int load", "a = \"NodeId\"::-10", "a", NodeCons (SExpr(Str "NodeId"), SExpr(Int -10)) ;
    // "Int identifier, bool load", "a = 0::True", "a", BE (Int 0) NodeCons (Bool true);
    // "Var identifier, var load", "a = b::c", "a", BE (Var "b") NodeCons (Var "c");
]