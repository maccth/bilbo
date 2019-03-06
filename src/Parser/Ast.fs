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
    | ObjExpr of ObjInstantiation


and BinExpr = Expr * BinOp * Expr 
 
and ObjInstantiation = TypeName * Expr list

and BinOp =
  | Plus
  | Minus
  | Times
  | Divide

and Literal =
    | StrLit of string
    | FloatLit of float
    | IntLit of int
    | BoolLit of bool
      
and VarId = string
and TypeName = string
and Attribute = string 
