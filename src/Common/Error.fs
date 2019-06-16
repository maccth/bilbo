module Bilbo.Common.Error

open Bilbo.Common.Ast
open Bilbo.Common.Result

let bilboErrorStr (be : BilboError) =
    match be with
    | ImplementationError e -> "Implementation error:" + " " + e 
    | SyntaxError e -> "Syntax error:" + " " + e
    | FieldError e -> "Field error:" + " " + e
    | NameError e -> "Name error:" + " " + e
    | ValueError e -> "Value error:" + " " + e
    | TypeError e -> "Type error:" + " " + e
    | OperatorError e -> "Operator error:" + " " + e
    | MatchError e -> "Match error:" + " " + e
    | ImportError e -> "Import error:" + " " + e

let locStr (loc:Loc) =
    loc.file +
    " around " + string(loc.startLine) + ":" + string(loc.startCol)
    + " - " + string(loc.endLine) + ":" + string(loc.endCol)

let extraInfoStr (extra : (string * Loc option) list) =
    let mapper (eStr,loc) =
        match loc with
        | None -> "\n\t" + eStr
        | Some l -> "\n\t" + eStr + " " + (l |> locStr)
    List.map mapper extra    

let prettyError (e : ProgramError) =
    let mainError = e.error |> bilboErrorStr
    let mainLoc =
        match e.loc with
        | None -> ""
        | Some l -> l |> locStr
    let extras =
        e.extra
        |> extraInfoStr 
        |> function
        | [] -> ""
        | s -> s |> List.reduce (+)
    mainError + " " + mainLoc + " " + extras

type ProgramResult<'T> = Result<'T,string>