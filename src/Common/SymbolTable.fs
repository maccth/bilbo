module Bilbo.Common.SymbolTable

open Bilbo.Common.Value
open Bilbo.Common.Ast
open Bilbo.Common.Error

type ValueId =
    {
        spLst : SpaceId list;
        id : Id;
    }

module SymbolTable =
    let empty : SymbolTable = Map.empty
       
    let find (symtab : SymbolTable) (vid : ValueId) : BilboResult<Meaning> =
        let rec findSpace (st : SymbolTable) (vid : ValueId) : BilboResult<SymbolTable> =
            match vid.spLst with
            | [] -> st |> Ok
            | Top :: rest -> findSpace st {vid with spLst = rest}
            | Name n :: rest ->
                match Map.tryFind n st with
                | Some (Space(_, stNext)) -> 
                    findSpace stNext {vid with spLst = rest}
                | _ ->
                    "Namespace " + "\"" + n + "\"" + " is not defined"
                    |> NameError
                    |> Error
        let findMeaning (st : SymbolTable) (vid : ValueId) : BilboResult<Meaning> =
            match Map.tryFind vid.id st with
            | Some v -> v |> Ok
            | _ ->
                "Field " + "\"" + vid.id + "\"" + " is not is not defined"
                |> NameError
                |> Error              
        vid
        |> findSpace symtab
        |> Result.bind (fun st' -> findMeaning st' vid)

    let set (symtab : SymbolTable) (vid : ValueId) (value : Meaning) (*: BilboResult<Table list>*) =
        let setMeaning (st : SymbolTable) (vid : ValueId) =
            Map.add vid.id value st
            
        let rec setSpace (st : SymbolTable) (vid : ValueId) : BilboResult<SymbolTable> =
            match vid.spLst with
            | [] ->
                setMeaning st vid
                |> Ok                          
            | Top :: rest ->
                setSpace st {vid with spLst=rest}
            | Name n :: rest ->
                match Map.tryFind n st with
                | Some (Space(spType, stExisting)) ->                 
                    let stUpdated = setSpace stExisting {vid with spLst=rest} 
                    match stUpdated with
                    | Ok stU' -> Map.add n ((spType,stU') |> Space) st |> Ok
                    | Error e -> e |> Error
                | Some (Value(Value.Node node)) ->
                    match node.load with
                    | Space(loadTyp, existingLoad) ->
                        let updatedLoad = setSpace existingLoad {vid with spLst=rest} 
                        match updatedLoad with
                        | Ok load' ->
                            let node' = 
                                {node with load=Space(loadTyp, load')}
                                |> Value.Node
                                |> Value
                            Map.add n node' st |> Ok
                        | Error e -> e |> Error
                    | _ ->
                        "Field access forwarding requires the node load to be of object type"
                        |> TypeError
                        |> Error                    
                | _ ->
                     "Name " + "\"" + n + "\"" + " is not is not defined"
                    |> NameError
                    |> Error

        setSpace symtab vid

    // TODO: Serious code sketch. Will need to permeate use of vid.spLst 
    let remove (symtab : SymbolTable) (vid : ValueId) =
        match Map.containsKey vid.id symtab with
        | false -> "Cannot delete an identifier that does not exist" |> TypeError |> Error
        | true -> Map.remove vid.id symtab |> Ok


type Symbols = SymbolTable list

module Symbols =
    let head (syms : Symbols) =
        match syms with
        | hd :: _ -> hd
        | [] -> SymbolTable.empty
        
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
            Result.bind (fun s -> s::rest |> Ok) st'
        | [] ->
            let st' = SymbolTable.set SymbolTable.empty vid value
            Result.bind (fun s -> [s] |> Ok) st'

    let remove (syms : Symbols) (vid : ValueId) : BilboResult<Symbols> =
        let st' = SymbolTable.remove (head syms) vid
        let swapHead newHd =
            match syms with
            | hd :: rest -> newHd :: rest |> Ok
            | [] -> [newHd] |> Ok
        Result.bind swapHead st'