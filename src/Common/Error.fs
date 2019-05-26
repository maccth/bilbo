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

type BilboError =
    | ImplementationError of ImplementationError
    | SyntaxError of SyntaxError
    | FieldError of FieldError
    | NameError of NameError
    | ValueError of ValueError
    | TypeError of TypeError
    | OperatorError of OperatorError
    | MatchError of MatchError

type BilboResult<'T> = Result<'T, BilboError>

type ProgramError = Loc option * BilboError

type ProgramResult<'T> = Result<'T,ProgramError>

let implementationError (m : string) = m |> ImplementationError |> Error

let parseError file msg err state : BilboResult<Program> =
    let str = "In file: " + file + "\n" + msg
    str |> SyntaxError |> Error

let objInstanArgNumError typName (expNum : int) (givenNum : int) =
    "Object instantiation of type \"" + typName + "\""
    |> fun s -> s + " requires " + string(expNum)
    |> fun s -> s + " arguments, but given " + string(givenNum)
    |> fun s -> s + " arguments."
    |> TypeError |> Error

let nodeConsError vType nodePart =
    "A " + vType + " cannot be used as a " + nodePart
    |> TypeError
    |> Error

let bindTypeError thing cannotBeBoundTo =
    "Cannot bind a " + thing + " to " + cannotBeBoundTo
    |> TypeError
    |> Error

let bindImpError thing cannotBeBoundTo =
    "A" + thing + "should not be bound to" + cannotBeBoundTo
    |> implementationError

let paramListTypeError thing = bindTypeError "parameter list" thing

let paramListImpError thing = bindImpError  "parameter list" thing

let tooManyArguments count =
    count + " extra arguments enpiped into function, transform or pipeline."
    |> ValueError
    |> Error

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
    |> Error

let typeNotDefined typ =
    "Type " + "\"" + typ + "\" is not defined."
    |> NameError
    |> Error

let notMatchingWithinGraph typ =
    "Can only match within graphs but attempted to match within " + typ + " type."
    |> TypeError
    |> Error

let nonNodeInPathEdge typL typR =
    let mes =
        "Only nodes can appear on either side of an edge. "
        + "Attempted to create an edge with type "
        + typL + " on the left and type "
        + typR + " on the right."
    mes
    |> TypeError
    |> Error    

let nonNodeInPath typ =
    let mes =
        "Only nodes or edges can be used as elements of path expressions. "
        + "Attempted to use "
        + typ + "."
    mes
    |> TypeError
    |> Error 

let nonPStageOnEnpipeRhs typR =
    "The enpipe operator requires a function, transform or pipeline on the "
    + "right-hand side but instead had " + typR +  "."
    |> TypeError
    |> Error

let nonBoolInWhereClause typ =
    "The value of a where clause must evaluate to a bool however it "
    + "evaluated to a " + typ + "."
    |> TypeError
    |> Error

let typeCastError typeFrom typeTo =
    "Cannot convert type " + typeFrom + " to type " + typeTo
    |> TypeError
    |> Error

let typeCastValueError typeFrom typeTo =
    "Cannot convert this value of type " + typeFrom + " to type " + typeTo
    |> ValueError
    |> Error

let nonGraphCollectionError typeL typeR =
    "Only graphs can be placed in collections. "
    + "Attempted type " + typeL + " and type " + typeR + "."
    |> TypeError
    |> Error

let nonIntMultipleAppRhs typR =
    "The ** operator requires a positive integer on the right-hand side. "
    + "Instead got type " + typR + "."
    |> TypeError
    |> Error

let nonPipelineDollarApp typ =
    "The $ operator requires a transform or pipeline on the right-hand side. "
    + "Instead got type " + typ + "."
    |> TypeError
    |> Error

let nonPipelineMultipleAppLhs typL =
    "The ** operator requires a function, transform or pipeline on the left-hand side. "
    + "Instead got type " + typL + "."
    |> TypeError
    |> Error

let nonPipelineUnfinished typ =
    "Only pipelines can be an unfinished output. "
    + "This was an unfinished of type " + typ
    |> implementationError

let nonPositiveAppMultiplier m =
    "The ** operator requires a positive integer on the right-hand side. "
    + "Instead got " + m + "."
    |> ValueError
    |> Error

let alapMaybeAppNonPLine typ =
    "The ! and ? pipeline modifiers must be applied to a transform or pipeline. "
    + "Instead got " + typ + "."
    |> TypeError
    |> Error

let functionAlapError() =
    "Functions cannot be applied as-long-as-possible (ALAP)"
    |> TypeError
    |> Error   

let nonNodeAmpExpr typ =
    "The & operator must be applied to a node. "
    + "Attempted to apply it to a type " + typ + "."
    |> TypeError
    |> Error

let nonNodeDblAmpExpr typ =
    "The && operator must be applied to a node. "
    + "Attempted to apply it to a type " + typ + "."
    |> TypeError
    |> Error

let binOpTypeError opSymbol typL typR =
    "The " + opSymbol + " cannot be used with the types "
    + typL + " and " + typR + "."
    |> TypeError
    |> Error