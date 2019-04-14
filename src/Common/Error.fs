module Bilbo.Common.Error

open Bilbo.Common.Ast

let Pass = Ok
let Fail = Error

type SyntaxError = string
type ImplementationError = string
type NameError = string
type FieldError = string

type BilboError =
    | SyntaxError of SyntaxError
    | FieldError of FieldError
    | NameError of NameError
    | ImplementationError of ImplementationError

type BilboResult<'T> = Result<'T, BilboError>

let parseError file msg err state : BilboResult<Program> =
    let str = "In file: " + file + "\n" + msg
    str |> SyntaxError |> Error