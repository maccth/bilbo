module Bilbo.Parser.Ast
open System.Linq.Expressions

type Program = ProgramUnit list

and ProgramUnit =
    | Statement of Statement

and Statement =
    | TypeDeclaration of TypeName * Attribute list
    | ExpressionStatement of ExpressionStatement

and ExpressionStatement =
    | AssignmentExpression of VariableIdentifier * Expression

and Expression =
    | LiteralExpression of Literal
    | ObjectExpression of ObjectInstantiation
 
and ObjectInstantiation = TypeName * Expression list

and Literal =
    | StringLiteral of string
    | FloatLiteral of float
    | IntLiteral of int
    | BoolLiteral of bool
      
and VariableIdentifier = string
and TypeName = string
and Attribute = string 
