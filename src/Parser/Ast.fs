module Bilbo.Parser.Ast

type Program = ProgramUnit list

and ProgramUnit =
    | Statement of Statement

and Statement =
    | TypeDeclaration of TypeName * Attribute list
    | ExprStatement of ExprStatement

and ExprStatement =
    | AssignmentExpr of VariableIdentifier * Expr

and Expr =
    | BinaryExpr of BinaryExpr
    | LiteralExpr of Literal
    | ObjectExpr of ObjectInstantiation


and BinaryExpr = Expr * BinaryOperator * Expr 
 
and ObjectInstantiation = TypeName * Expr list

and BinaryOperator =
  | Plus
  | Minus
  | Times
  | Divide

and Literal =
    | StringLiteral of string
    | FloatLiteral of float
    | IntLiteral of int
    | BoolLiteral of bool
      
and VariableIdentifier = string
and TypeName = string
and Attribute = string 
