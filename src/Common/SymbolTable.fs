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

type SymbolTable = Map<Id, Meaning>

and Meaning =
    | Value of Value
    | Space of SpaceType * SymbolTable 

and SpaceType =
    | Namespace
    | Objectspace

module SymbolTable =

    let empty : SymbolTable = Map.empty
       
    let find (symtab : SymbolTable) (vid : ValueId) : BilboResult<Meaning> =
        let rec findNspace (st : SymbolTable) (vid : ValueId) : BilboResult<SymbolTable> =
            match vid.nLst with
            | [] -> st |> Ok
            | Top :: rest -> findNspace st {vid with nLst = rest}
            | Name n :: rest ->
                match Map.tryFind n st with
                | Some (Space(Namespace, stNext)) -> 
                    findNspace stNext {vid with nLst = rest}
                | _ ->
                    "Namespace " + "\"" + n + "\"" + " is not is not defined"
                    |> NameError
                    |> Error
        let findMeaning (st : SymbolTable) (vid : ValueId) : BilboResult<Meaning> =
            let rec findOspace (st : SymbolTable) (vid : ValueId) : BilboResult<Meaning> =
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
                    | Some (Space (typ,st')) -> 
                        findOspace st' {vid with oLst = rest}
                    | _ ->
                        "Field " + "\"" + o + "\"" + " is not is not defined"
                        |> NameError
                        |> Error  
            let findValue (st : SymbolTable) : BilboResult<Meaning> =
                match vid.oLst with
                | [] ->
                    match Map.tryFind vid.id st with
                    | Some v -> v |> Ok
                    | _ ->
                        "Name " + "\"" + vid.id + "\"" + " is not is not defined"
                        |> NameError
                        |> Error
                | _ -> findOspace st vid
            vid
            |> findNspace st
            |> Result.bind findValue
        match vid.nLst with
        // | [] ->
        //     "All program units exist in a namespace."
        //     |> ImplementationError
        //     |> Error
        //     |> failwithf "%A"
        | _ ->
            let symVal = findMeaning symtab vid
            match symVal with
            | Ok value ->
                value |> Ok
            | Error e ->
                e |> Error

    let set (symtab : SymbolTable) (vid : ValueId) (value : Meaning) (*: BilboResult<Table list>*) =
        let rec setObj (st : SymbolTable) (vid : ValueId) =
            match vid.oLst with
            | [] -> Map.add vid.id value st
            | o :: rest ->
                match Map.tryFind o st with
                | Some (Space(Objectspace,stExisting)) ->
                    // Used to be setNspace
                    let stUpdated = setObj stExisting {vid with oLst=rest} |> fun s -> (Objectspace,s) |> Space
                    Map.add o stUpdated st
                | _ ->
                    // Used to be setNspace
                    let stNew = setObj empty {vid with oLst=rest} |> fun s -> (Objectspace,s) |> Space
                    Map.add o stNew st
        and setNspace (st : SymbolTable) (vid : ValueId) (*: BilboResult<Table>*) =
            match vid.nLst with
            | [] -> setObj st vid                                         
            | Top :: rest -> setNspace st {vid with nLst=rest}
            | Name n :: rest ->
                match Map.tryFind n st with
                | Some (Space(Namespace, stExisting)) ->                 
                    let stUpdated = setNspace stExisting {vid with nLst=rest} |> fun s -> (Namespace,s) |> Space
                    Map.add n stUpdated st
                | _ ->
                    let stNew = setNspace empty {vid with nLst=rest} |> fun s -> (Namespace,s) |> Space
                    Map.add n stNew st
        setNspace symtab vid


type Symbols = SymbolTable list

module Symbols =
    let empty : Symbols = [] 
    let find (syms : Symbols) (vid : ValueId) : BilboResult<Meaning> =
        let rec findClosest (symsIn : Symbols) (errLst : BilboError list) : BilboResult<Meaning> =
            match symsIn with
            | st :: rest ->
                match SymbolTable.find st vid with
                | Ok value -> value |> Ok
                | Error e -> findClosest rest (e::errLst)
            | [] ->
                // TODO: generate specific error from errList
                // let errLst' = List.rev errLst
                "Name " + "\"" + vid.id + "\"" + " is not is not defined"
                |> NameError
                |> Error
        findClosest syms []
    let set (syms : Symbols) (vid : ValueId) (value : Meaning) : BilboResult<Symbols> =
        match syms with
        | st :: rest ->
            let st' = SymbolTable.set st vid value
            st'::rest
            |> Ok
        | [] ->
            let st' = SymbolTable.set SymbolTable.empty vid value
            [st']
            |> Ok

