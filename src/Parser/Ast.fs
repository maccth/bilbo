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
    | ObjExpr of ObjExpr
    | LitExpr of Literal
    | PathExpr of PathExpr
    // | NegExpr of Expr

and PathExpr =
    | Path of PathElem list

and PathElem =
    // Not correct, will need to be node expr for Bilbo :: operator
    | Node of Expr
    | Edge of EdgeOp    

and EdgeOp =
    // Edges can be weighted or unweighted
    | Right of Expr option
    | Left of Expr Option
    | Bidir of Expr Option

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
