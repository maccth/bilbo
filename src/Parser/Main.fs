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
    cli argv "Bilbo parsing REPL" bilboStringParserPrint bilboParser
    0