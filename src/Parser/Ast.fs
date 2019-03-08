module Bilbo.Parser.Ast

type Program = ProgramUnit list

and ProgramUnit =
    | Statement of Statement

and Statement =
    | TypeDeclaration of TypeName * Attribute list
    | ExprStatement of ExprStatement
    
and ExprStatement =
    | AssignmentExpr of Id * Expr

and Expr =
    | Var of Id
    | BinExpr of BinExpr
    | LitExpr of Literal
    | ObjExpr of ObjExpr
    | NegExpr of Expr

and BinExpr = Expr * BinOp * Expr 
 
and ObjExpr =
    | DotAccess of Expr * Id
    | ObjInstan of TypeName * Expr list

and BinOp =
  | Plus
  | Minus
  | Times
  | Divide
  | Pow

and Literal =
    | StrLit of string
    | FloatLit of float
    | IntLit of int
    | BoolLit of bool
      
and Id = string
and TypeName = string
and Attribute = string 
