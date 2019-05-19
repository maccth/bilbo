module Bilbo.Common.Extensions

let (|->) l r = Result.bind (r) l
let (|=>) l r = Result.bind (r >> Ok) l

type Match<'T> =
    | Matched of 'T
    | NoMatch

module Match =
    let underlie instead m =
        match m with
        | Matched m' -> m'
        | NoMatch -> instead

    let compose first second =
        match first with
        | Matched x -> x |> Matched
        | NoMatch ->
            match second with
            | Matched y -> y |> Matched
            | NoMatch -> NoMatch

    let lcompose (first : Match<'T>) (second : Lazy<Match<'T>>) =
        match first with
        | Matched x -> x |> Matched
        | NoMatch ->
            match second.Force() with
            | Matched y -> y |> Matched
            | NoMatch -> NoMatch

let (|?>) first second = Match.compose first second

let (<?|?>) first second = Match.compose first second

let (|??>) first second = Match.lcompose first second

let (<??|??>) first second = Match.lcompose first second

let (|..>) m instead = Match.underlie instead m