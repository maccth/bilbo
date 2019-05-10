module Bilbo.Parser.Main

open Bilbo.Common.Cli
open Bilbo.Parser.Parser

[<EntryPoint>]
let main (argv : string []) =
    let file = bilboParser
    let repl codeIn _prevAst = bilboStringParser codeIn
    cli argv "Bilbo parsing REPL" file repl []
    0