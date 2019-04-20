module Bilbo.Common.Error

open Bilbo.Common.Ast

let Pass = Ok
let Fail = Error

type SyntaxError = string
type FieldError = string
type NameError = string
type ImplementationError = string
type ValueError = string
type TypeError = string
type OperatorError = string

type BilboError =
    | SyntaxError of SyntaxError
    | FieldError of FieldError
    | NameError of NameError
    | ImplementationError of ImplementationError
    | ValueError of ValueError
    | TypeError of TypeError
    | OperatorError of OperatorError

type BilboResult<'T> = Result<'T, BilboError>

type ProgramError = Loc option * BilboError

type ProgramResult<'T> = Result<'T,ProgramError>

let parseError file msg err state : BilboResult<Program> =
    let str = "In file: " + file + "\n" + msg
    str |> SyntaxError |> Error

let objInstanArgNumError typName (expNum : int) (givenNum : int) =
    "Object instantiation of type \"" + typName + "\""
    |> fun s -> s + " requires " + string(expNum)
    |> fun s -> s + " arguments, but given " + string(givenNum)
    |> fun s -> s + " arguments."
    |> TypeError |> Error