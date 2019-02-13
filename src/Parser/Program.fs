// Learn more about F# at http://fsharp.org

open System
open FParsec

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let a = pstring "*" |> between (pstring "(") (pstring ")")
    0
