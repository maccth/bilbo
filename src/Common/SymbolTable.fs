module Bilbo.Common.SymbolTable

open Bilbo.Common.Value
open Bilbo.Common.Ast
open Bilbo.Common.Error

type ValueId =
    {
        nLst : Namespace list;
        oLst : Id list;
        id : Id;
    }

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
        match nLst with
        | [] ->
            "All program units exist in a namespace."
            |> ImplementationError
            |> Error
            |> failwithf "%A"
        | _ ->
            match symTabs with
            | st :: rest ->
                let st' = add st nLst id def'
                st' :: rest
            | [] ->
                let st' = add empty nLst id def'
                [st']
   
    let getSymbolValue (symTabs : Table list) (vid : ValueId) : BilboResult<Meaning> =
        let rec getField (st : Table) (vid : ValueId) : BilboResult<Meaning> =
            // match st with
            // | SymbolTable (Object,st) ->
            match vid.oLst with
            | [] ->
                match Map.tryFind vid.id st with
                | Some value -> value |> Ok
                | _ ->
                    "Field " + "\"" + vid.id + "\"" + " is not is not defined"
                    |> NameError
                    |> Error
            | o :: rest ->
                match Map.tryFind o st with
                | Some (SymbolTable (typ,st')) -> 
                    getField st' {vid with oLst = rest}
                | _ ->
                    "Field " + "\"" + o + "\"" + " is not is not defined"
                    |> NameError
                    |> Error                   
        let rec getSpace (st : Table) (vid : ValueId) : BilboResult<Table> =
            match vid.nLst with
            | [] -> st |> Ok
            | Top :: rest ->
                getSpace st {vid with nLst = rest}
            | Name n :: rest ->
                match Map.tryFind n st with
                | Some (SymbolTable(Namespace, stNext)) -> 
                    getSpace stNext {vid with nLst = rest}
                | _ ->
                    "Namespace " + "\"" + n + "\"" + " is not is not defined"
                    |> NameError
                    |> Error
        let get (st : Table) (vid : ValueId) : BilboResult<Meaning> =
            let sp = vid |> getSpace st
            let getValue (st : Table) =
                match vid.oLst with
                | [] ->
                    match Map.tryFind vid.id st with
                    | Some v -> v |> Ok
                    | _ ->
                        "Name " + "\"" + vid.id + "\"" + " is not is not defined"
                        |> NameError
                        |> Error
                | _ -> getField st vid
            Result.bind getValue sp
        match vid.nLst with
        | [] ->
            "All program units exist in a namespace."
            |> ImplementationError
            |> Error
            |> failwithf "%A"
        | _ ->
            let st = 
                match symTabs with
                | st :: _ -> st
                | [] -> empty
            let symVal = get st vid
            match symVal with
            | Ok value ->
                value |> Ok
            | Error e ->
                e |> Error

    let setSymbolValue (symTabs : Table list) (vid : ValueId) (value : Meaning) (*: BilboResult<Table list>*) =
        let (st,rest) =
            match symTabs with
            | hd :: rest -> hd,rest
            | [] -> (empty,[])
        let rec set (st : Table) (vid : ValueId) (*: BilboResult<Table>*) =
            match vid.nLst with
            | [] ->
                let rec setObj (st : Table) (vid : ValueId) =
                    match vid.oLst with
                    | [] ->
                        Map.add vid.id value st
                    | o :: rest ->
                        match Map.tryFind o st with
                        | Some (SymbolTable(Object,stCurrent)) ->
                            let stUpdated = set stCurrent {vid with oLst=rest} |> fun s -> (Object,s) |> SymbolTable
                            Map.add o stUpdated st
                        | _ ->
                            let stNew = set empty {vid with oLst=rest} |> fun s -> (Object,s) |> SymbolTable
                            Map.add o stNew st                              
                setObj st vid                                         
            | Top :: rest -> set st {vid with nLst=rest}
            | Name n :: rest ->
                match Map.tryFind n st with
                | Some (SymbolTable(Namespace, stCurrent)) ->                 
                    let stUpdated = set stCurrent {vid with nLst=rest} |> fun s -> (Namespace,s) |> SymbolTable
                    Map.add n stUpdated st
                | _ ->
                    let stNew = set empty {vid with nLst=rest} |> fun s -> (Namespace,s) |> SymbolTable
                    Map.add n stNew st
        let st' = set st vid
        st' :: rest

type ProgramSymbols = SymbolTable.Table list