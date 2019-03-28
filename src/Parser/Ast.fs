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
    | SExpr of SExpr
    | GExpr of GExpr

and SExpr =
    | Var of Id
    | BinExpr of BinExpr
    | ObjExpr of ObjExpr
    | LitExpr of Literal
    | NodeCons of NodeCons

and BinExpr = SExpr * BinOp * SExpr 
 
and NodeCons = SExpr * SExpr

and ObjExpr =
    | DotAccess of SExpr * Id
    | ObjInstan of TypeName * SExpr list

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
      
and GExpr =
    | Var of Id
    | PathExpr of PathExpr

and PathExpr =
    | Path of PathElem list

and PathElem =
    | Node of NodeExpr
    | Edge of EdgeOp
    // | Edge of NodeExpr * EdgeOp * NodeExpr
 
and NodeExpr =
    | Var of Id
    | NodeCons of NodeCons

and EdgeOp =
    // Edges can be weighted or unweighted
    | Right of SExpr option
    | Left of SExpr Option
    | Bidir of SExpr Option

// Type definitions
and Id = string
and TypeName = string
and Attribute = string 
