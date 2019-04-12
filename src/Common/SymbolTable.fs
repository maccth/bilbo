module Bilbo.Common.SymbolTable

open Bilbo.Common.Value
open Bilbo.Common.Ast

module SymbolTable =
    type Table = Map<Id, Meaning>

    and Meaning =
        | Value of Value
        | SymbolTable of SymbolType * Table 

    and SymbolType =
        | Namespace
        | Object

    let empty : Table
        = Map.empty
    
    let addType (symTabs:Table list) nLst id (def : TypeDef) =
        let rec add (st : Table) (nLst : Namespace list) (id : Id) (def : Meaning) =
            match nLst with
            | [] ->
                Map.add id def st
            | Top :: rest ->
                add st rest id def
            | Name n :: rest ->
                match Map.tryFind n st with
                | Some (SymbolTable(Namespace, stCurrent)) ->                 
                    let stUpdated = add stCurrent rest id def |> fun s -> SymbolTable(Namespace,s)
                    Map.add n stUpdated st
                | _ ->
                    let stNew = (add empty rest id def) |> fun s -> (Namespace,s) |> SymbolTable
                    Map.add n stNew st
        let def' = def |> Type |> Value
        let rnLst = List.rev nLst
        match rnLst with
        | e :: nLst' ->
            match symTabs with
            | st :: rest ->
                let st' = add st rnLst id def'
                st' :: rest
            | [] ->
                let st' = add empty rnLst id def'
                [st']
        | [] -> failwithf "Will not happen. All program units exist in a namespace." 

type ProgramSymbols = SymbolTable.Table list