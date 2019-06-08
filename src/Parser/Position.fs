module Bilbo.Parser.Position

open FParsec
open Bilbo.Common.Ast
open Bilbo.Common.Result

// FParsec line and column number
// ==============================
// credit : https://stackoverflow.com/questions/55590902/fparsec-keeping-line-and-column-numbers
// edited by maccth

type WithPos<'T> = (Loc * 'T)

module Position =
    /// Get the previous position on the same line.
    let leftOf (p: Position) =
        if p.Column > 1L then
            Position(p.StreamName, p.Index - 1L, p.Line, p.Column - 1L)
        else
            p

    /// Wrap a parser to include the position
    let attachPos fname (p: Parser<'T, 'U>) : Parser<WithPos<'T>, 'U> =
        // Get the position before and after parsing
        pipe3 getPosition p getPosition <| fun start value finish ->
            let loc = {
                file = fname;
                startLine = start.Line;
                startCol = start.Column;
                endLine = (leftOf finish).Line;
                endCol = (leftOf finish).Column;
            }
            (loc, value)

    let bind binder (p : Parser<WithPos<'T>, 'U>) =
        p |>> binder