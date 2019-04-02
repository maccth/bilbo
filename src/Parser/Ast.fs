module Bilbo.Parser.Ast

type Program = ProgramUnit list

and ProgramUnit =
    | Statement of Statement

and Statement =
    | TypeDef of TypeDef
    | TransformDef of TransformDef
    | ExprStatement of ExprStatement

and TypeDef = TypeName * Attribute list
    
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


and TransformDef =
    Id * (Param list) * (ExprStatement list) * MatchStatement

and MatchStatement =
    Option<GExpr> * MatchCase list

and MatchCase =
    MExpr * ExprStatement list * TerminatingStatement

and MExpr =
    | GExpr of GExpr
    | BinExpr of MExpr * MBinOp * MExpr

and MBinOp =
    | Not // Prefix
    | And // Infix

and TerminatingStatement =
    | Return of Expr
    | Become of Expr

// Helpful type definitions to increase AST readability
and Id = string
and Param = string
and TypeName = string
and Attribute = string 