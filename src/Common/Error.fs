module Bilbo.Common.Error

open Bilbo.Common.Ast

let Pass = Ok
let Fail = Error

type SyntaxError = string
type FieldError = string
type NameError = string
type ImplementationError = string
type ValueError = string

type BilboError =
    | SyntaxError of SyntaxError
    | FieldError of FieldError
    | NameError of NameError
    | ImplementationError of ImplementationError
    | ValueError of ValueError

type BilboResult<'T> = Result<'T, BilboError>

let parseError file msg err state : BilboResult<Program> =
    let str = "In file: " + file + "\n" + msg
    str |> SyntaxError |> Error
