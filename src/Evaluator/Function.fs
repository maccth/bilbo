module Bilbo.Evaluator.Function

open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.SymbolTable
open Bilbo.Common.Error
open Bilbo.Evaluator.PrimativeTypes
open Bilbo.Evaluator.BinaryExpressions
open Bilbo.Common

let evalfunctionDef (syms : Symbols) spLst (f : FunctionDef) : BilboResult<Symbols> =
    let fName,_,_,_ = f
    let fst =
        match syms with
        | hd :: rest -> hd
        | [] -> SymbolTable.empty
    let p = (f, fst) |> Function |> fun s -> [s] |> Pipeline |> Value
    Symbols.set syms {id=fName; spLst=spLst} p
