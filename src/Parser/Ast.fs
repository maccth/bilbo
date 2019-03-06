module Bilbo.Parser.Ast

type Program = ProgramUnit list

and ProgramUnit =
    | Statement of Statement

and Statement =
    | TypeDeclaration of TypeName * Attribute list
    | ExprStatement of ExprStatement
    
and ExprStatement =
    | AssignmentExpr of VarId * Expr

and Expr =
    | BinExpr of BinExpr
    | LitExpr of Literal
    | ObjExpr of ObjExpr
    | NegExpr of Expr

and BinExpr = Expr * BinOp * Expr 
 
and ObjExpr =
    | Var of VarId
    | DotAccess of VarId * VarId
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
      
and VarId = string
and TypeName = string
and Attribute = string 
