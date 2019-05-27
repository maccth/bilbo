module Bilbo.Common.SymbolTable

open Bilbo.Common.Ast
open Bilbo.Common.Value
open Bilbo.Common.Error

module SymbolTable =
    let empty : SymbolTable = Map.empty
       
    let find (symtab : SymbolTable) (vid : ValueId) : BilboResult<Meaning> =
        let rec findSpace (st : SymbolTable) (vid : ValueId) : BilboResult<SymbolTable> =
            match vid.spLst with
            | NodePart _ :: _ -> "NodePart SpaceIds should not be used find objects in the symbol table" |> implementationError
            | [] -> st |> Ok
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
                "Field " + "\"" + vid.id + "\"" + " is not defined"
                |> NameError
                |> Error              
        vid
        |> findSpace symtab
        |> Result.bind (fun st' -> findMeaning st' vid)

    let set (symtab : SymbolTable) (vid : ValueId) (value : Meaning) =
        let setMeaning (st : SymbolTable) (vid : ValueId) =
            Map.add vid.id value st
        
        let rec setNodePart (st : SymbolTable) (vid : ValueId) spLstRest (partId : Id option) getPart update spaceUpdate =
            match partId with
            | None ->
                match Map.tryFind vid.id st with
                | Some (Value(Node node)) ->
                    let changedNode = update node value |> Node |> Value
                    Map.add vid.id changedNode st
                    |> Ok
                | _ -> "No node there" |> NameError |> Error
            | Some nodeId ->
                match Map.tryFind nodeId st with
                | Some (Value(Node node)) ->
                    match getPart node with
                    | Space (typ,st') ->
                        let st'' = setSpace st' {id=vid.id; spLst=spLstRest}
                        match st'' with
                        | Error e -> e |> Error
                        | Ok id' ->
                            let node' = spaceUpdate node typ id' |> Node |> Value
                            Map.add nodeId node' st
                            |> Ok
                    | _ -> "No space there" |> NameError |> Error                            
                | _ -> "Again no node there" |> NameError |> Error
            
        and setSpace (st : SymbolTable) (vid : ValueId) : BilboResult<SymbolTable> =
            match vid.spLst with
            | [] -> setMeaning st vid |> Ok

            | NodePart (IdSpace, id) :: rest ->
                let spaceUpdate node typ idSpace = {node with Node.id = (typ,idSpace) |> Space}               
                setNodePart st vid rest id (fun n -> n.id) (fun n v -> {n with id = v}) spaceUpdate 
            | NodePart (LoadSpace,load) :: rest ->
                let spaceUpdate node typ idSpace = {node with Node.load = (typ,idSpace) |> Space}               
                setNodePart st vid rest load (fun n -> n.load) (fun n v -> {n with load = v}) spaceUpdate
            | Name n :: rest ->
                match Map.tryFind n st with
                | Some (Space(spType, stExisting)) ->                 
                    let stUpdated = setSpace stExisting {vid with spLst=rest} 
                    match stUpdated with
                    | Ok stU' -> Map.add n ((spType,stU') |> Space) st |> Ok
                    | Error e -> e |> Error

                // | Some (Value(Value.Node node)) ->
                //     match node.load with
                //     | Space(loadTyp, existingLoad) ->
                //         let updatedLoad = setSpace existingLoad {vid with spLst=rest} 
                //         match updatedLoad with
                //         | Ok load' ->
                //             let node' = 
                //                 {node with load=Space(loadTyp, load')}
                //                 |> Value.Node
                //                 |> Value
                //             Map.add n node' st |> Ok
                //         | Error e -> e |> Error
                //     | _ ->
                //         "Field access forwarding requires the node load to be of object type"
                //         |> TypeError
                //         |> Error
                                    
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

    let top (syms : Symbols) =
        match syms with
        | hd :: rest -> hd,rest
        | [] ->  SymbolTable.empty, [SymbolTable.empty]
        
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