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
    type Bindings = Map<Id, Meaning>

    and Meaning =
        | Value of Value
        | Space of SpaceType * Bindings 

    and SpaceType =
        | Namespace
        | Objectspace

    let empty : Bindings = Map.empty
    
    let addType (symTabs : Bindings list) nLst id (def : TypeDef) =
        let rec add (st : Bindings) (nLst : Namespace list) (id : Id) (def : Meaning) =
            match nLst with
            | [] ->
                Map.add id def st
            | Top :: rest ->
                add st rest id def
            | Name n :: rest ->
                match Map.tryFind n st with
                | Some (Space(Namespace, stExisting)) ->                 
                    let stUpdated = add stExisting rest id def |> fun s -> Space(Namespace,s)
                    Map.add n stUpdated st
                | _ ->
                    let stNew = (add empty rest id def) |> fun s -> (Namespace,s) |> Space
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
   
    let find (symtab : Bindings) (vid : ValueId) : BilboResult<Meaning> =
        let rec findNspace (st : Bindings) (vid : ValueId) : BilboResult<Bindings> =
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
        let findMeaning (st : Bindings) (vid : ValueId) : BilboResult<Meaning> =
            let rec findOspace (st : Bindings) (vid : ValueId) : BilboResult<Meaning> =
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
            let findValue (st : Bindings) : BilboResult<Meaning> =
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

    let set (symTabs : Bindings list) (vid : ValueId) (value : Meaning) (*: BilboResult<Table list>*) =
        let (st,rest) =
            match symTabs with
            | hd :: rest -> hd,rest
            | [] -> (empty,[])
        let rec setObj (st : Bindings) (vid : ValueId) =
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
        and setNspace (st : Bindings) (vid : ValueId) (*: BilboResult<Table>*) =
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
        let st' = setNspace st vid
        st' :: rest


module Symbols =
    type Bindings = SymbolTable.Bindings list
    let empty : Bindings = [] 
    let find (syms : Bindings) (vid : ValueId) : BilboResult<SymbolTable.Meaning> =
        let rec findClosest (symsIn : Bindings) (errLst : BilboError list) : BilboResult<SymbolTable.Meaning> =
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
