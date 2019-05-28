module Bilbo.Common.Error

open Bilbo.Common.Ast

type ImplementationError = string
type SyntaxError = string
type FieldError = string
type NameError = string
type ValueError = string
type TypeError = string
type OperatorError = string
type MatchError = string
type ImportError = string

type BilboError =
    | ImplementationError of ImplementationError
    | SyntaxError of SyntaxError
    | FieldError of FieldError
    | NameError of NameError
    | ValueError of ValueError
    | TypeError of TypeError
    | OperatorError of OperatorError
    | MatchError of MatchError
    | ImportError of ImportError

type ProgramError = {
    error   : BilboError
    loc     : Loc option
    extra   : (string * Loc option) list
}

type BilboResult<'T> = Result<'T, ProgramError>

module BilboError =
    let ofError be =
        {error=be; loc=None; extra=[]}
        |> Error

    let ofErrorLoc be loc =
        {error=be; loc=Some loc; extra=[]}
        |> Error
     
    let addExtra str pe =
        {pe with extra = (str,None) :: pe.extra}
        |> Error
    
    let addExtraLoc str loc pe =
        {pe with extra = (str, Some loc) :: pe.extra}
        |> Error
        
let (|NoMatches|_|) (res : BilboResult<'T>) =
    match res with
    | Error e ->
        match e.error with
        | MatchError me -> me |> Some
        | _ -> None
    | _ -> None

let implementationError (m : string) =
    m
    |> ImplementationError
    |> BilboError.ofError

let parseError file msg err state : BilboResult<Program> =
    let str = "In file: " + file + "\n" + msg
    str
    |> SyntaxError
    |> BilboError.ofError

let objInstanArgNumError typName (expNum : int) (givenNum : int) =
    "Object instantiation of type \"" + typName + "\""
    |> fun s -> s + " requires " + string(expNum)
    |> fun s -> s + " arguments, but given " + string(givenNum)
    |> fun s -> s + " arguments."
    |> TypeError |> BilboError.ofError

let nodeConsError vType nodePart =
    "A " + vType + " cannot be used as a " + nodePart
    |> TypeError
    |> BilboError.ofError

let bindTypeError thing cannotBeBoundTo =
    "Cannot bind a " + thing + " to " + cannotBeBoundTo
    |> TypeError
    |> BilboError.ofError

let bindImpError thing cannotBeBoundTo =
    "A" + thing + "should not be bound to" + cannotBeBoundTo
    |> implementationError

let paramListTypeError thing = bindTypeError "parameter list" thing

let paramListImpError thing = bindImpError  "parameter list" thing

let tooManyArguments count =
    count + " extra arguments enpiped into function, transform or pipeline."
    |> ValueError
    |> BilboError.ofError

let zeroParamFunctionError() =
    "Functions with no paramaters should be evaluated at definition time and cannot be enpiped to."
    |> implementationError

let zeroParamTransformError() =
    "Transforms with no paramaters are not valid and this should be caught at parse time."
    |> implementationError

let notImplementedYet thing =
    thing + " has not been implemented yet"
    |> implementationError

let printError thing=
    "Cannot print a " + thing
    |> TypeError
    |> BilboError.ofError

let typeNotDefined typ =
    "Type " + "\"" + typ + "\" is not defined."
    |> NameError
    |> BilboError.ofError

let notMatchingWithinGraph typ =
    "Can only match within graphs but attempted to match within " + typ + " type."
    |> TypeError
    |> BilboError.ofError

let invalidTypeInPathEdge typL typR =
    let mes =
        "An edge must have a node or graph on each side. "
        + "Attempted to create an edge with type "
        + typL + " on the left and type "
        + typR + " on the right."
    mes
    |> TypeError
    |> BilboError.ofError    

let nonNodeInPath typ =
    let mes =
        "Only nodes or edges can be used as elements of path expressions. "
        + "Attempted to use "
        + typ + "."
    mes
    |> TypeError
    |> BilboError.ofError 

let nonFuncTranInThenPipe typL typR =
    "Only functions or transforms can be composed in a pipeline with |>. "
    + "Attempted to compose a " + typL + " and a " + typR + "."
    |> TypeError
    |> BilboError.ofError

let nonTranInPipe pipeOp typL typR =
    "Only transforms can be composed in a pipeline with " + pipeOp + ". "
    + "Attempted to compose a " + typL + " and a " + typR + "."
    |> TypeError
    |> BilboError.ofError

let nonPStageOnEnpipeRhs typR =
    "The enpipe operator requires a function, transform or pipeline on the "
    + "right-hand side but instead had " + typR +  "."
    |> TypeError
    |> BilboError.ofError

let nonBoolInWhereClause typ =
    "The value of a where clause must evaluate to a bool however it "
    + "evaluated to a " + typ + "."
    |> TypeError
    |> BilboError.ofError

let typeCastError typeFrom typeTo =
    "Cannot convert type " + typeFrom + " to type " + typeTo
    |> TypeError
    |> BilboError.ofError

let typeCastValueError typeFrom typeTo =
    "Cannot convert this value of type " + typeFrom + " to type " + typeTo
    |> ValueError
    |> BilboError.ofError

let nonGraphCollectionError typeL typeR =
    "Only graphs can be placed in collections. "
    + "Attempted type " + typeL + " and type " + typeR + "."
    |> TypeError
    |> BilboError.ofError

let nonIntMultipleAppRhs typR =
    "The ** operator requires a positive integer on the right-hand side. "
    + "Instead got type " + typR + "."
    |> TypeError
    |> BilboError.ofError

let nonPipelineDollarApp typ =
    "The $ operator requires a transform or pipeline on the right-hand side. "
    + "Instead got type " + typ + "."
    |> TypeError
    |> BilboError.ofError

let nonPipelineMultipleAppLhs typL =
    "The ** operator requires a function, transform or pipeline on the left-hand side. "
    + "Instead got type " + typL + "."
    |> TypeError
    |> BilboError.ofError

let nonPipelineUnfinished typ =
    "Only pipelines can be an unfinished output. "
    + "This was an unfinished of type " + typ
    |> implementationError

let nonPositiveAppMultiplier m =
    "The ** operator requires a positive integer on the right-hand side. "
    + "Instead got " + m + "."
    |> ValueError
    |> BilboError.ofError

let alapMaybeAppNonPLine typ =
    "The ! and ? pipeline modifiers must be applied to a transform or pipeline. "
    + "Instead got " + typ + "."
    |> TypeError
    |> BilboError.ofError

let functionAlapError() =
    "Functions cannot be applied as-long-as-possible (ALAP)"
    |> TypeError
    |> BilboError.ofError   

let nonNodeAmpExpr typ =
    "The & operator must be applied to a node. "
    + "Attempted to apply it to a type " + typ + "."
    |> TypeError
    |> BilboError.ofError

let nonNodeHashExpr typ =
    "The && operator must be applied to a node. "
    + "Attempted to apply it to a type " + typ + "."
    |> TypeError
    |> BilboError.ofError

let binOpTypeError opSymbol typL typR =
    "The operator " + opSymbol + " cannot be used with the types "
    + typL + " and " + typR + "."
    |> TypeError
    |> BilboError.ofError

let divideModByZero() =
    "Cannot divide or modulo by zero."
    |> ValueError
    |> BilboError.ofError