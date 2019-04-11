module Bilbo.Common.SymbolTable

open Bilbo.Common.Value
open Bilbo.Common.Ast
open Bilbo.Common
open Bilbo.Common

module SymbolTable =
    type Table = Map<Id, Meaning>

    and Meaning =
        | Value of Value
        | SymbolTable of Table

    let empty : Table
        = Map.empty
    
    let addType (symTabs:Table list) nLst id (def : TypeDef) =
        let def' = def |> Type |> Value
        let rnLst = List.rev nLst
        match rnLst with
        | e :: nLst' ->
            let rec add (st : Table) (nLst : Namespace list) (id : Id) (def : Meaning) =
                match nLst with
                | Name n :: rest ->
                    match Map.tryFind n st with
                    | Some(SymbolTable stCurrent) ->
                        let stNew = add empty rest id def
                        let st' = Map.fold (fun acc k v -> Map.add k v acc) stCurrent stNew
                        st'
                    | _ ->
                        let nestedSt = add empty rest id def |> SymbolTable
                        Map.add n nestedSt st
                | Top :: rest ->
                    add st rest id def
                | [] ->
                    Map.add id def st
            match symTabs with
            | st :: rest ->
                let st' = add st rnLst id def'
                st' :: rest
            | [] ->
                let st' = add empty rnLst id def'
                [st']

type ProgramSymbols = SymbolTable.Table list