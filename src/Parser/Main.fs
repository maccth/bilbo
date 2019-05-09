module Bilbo.Parser.Main

open Bilbo.Common.Cli
open Bilbo.Parser.Parser

// TODO: Create `common` project for header files such as
//  - AST
//  - symbol table
//  - value type
//  - cli (below)


[<EntryPoint>]
let main (argv : string []) =
    let file = bilboParser
    let repl codeIn _prevAst = bilboStringParser codeIn
    cli argv "Bilbo parsing REPL" file repl []
    0