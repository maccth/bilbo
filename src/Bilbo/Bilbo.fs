module Bilbo.Bilbo.Bilbo

open Bilbo.Evaluator.Main

// This is the top level entry point for the whole interpreter
// While it just adds another level of indirection to the evaluator this means that the interpreter
//  is accessed from a Bilbo.dll rather than an Evaluator.dll which is a component of the system

[<EntryPoint>]
let main argv =
    bilboCli argv
    0