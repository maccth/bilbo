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
    | SVar of Id
    | BinExpr of SBinExpr
    | ObjExpr of ObjExpr
    | Literal of Literal
    | NodeCons of NodeCons
    // This can only be a var or NodeCons.
    // Errors will be found by semantic analyser
    | NodeId of SExpr

and SBinExpr = SExpr * SBinOp * SExpr 
 
and NodeCons = SExpr * SExpr

and ObjExpr =
    | DotAccess of SExpr * Id
    | ObjInstan of TypeName * SExpr list

and SBinOp =
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
    | GVar of Id
    | PathExpr of PathExpr
    | BinExpr of GExpr * GBinOp * GExpr

and GBinOp =
    | Plus
    | Minus

and PathExpr =
    | Path of PathElem list

and PathElem =
    | Node of NodeExpr
    | Edge of NodeExpr * EdgeOp * NodeExpr
 
and NodeExpr =
    | NVar of Id
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
