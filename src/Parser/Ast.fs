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
    | UnaryExpr of SUnaryOp * SExpr
    | ObjExpr of ObjExpr
    | Literal of Literal

and SBinExpr = SExpr * SBinOp * SExpr 
 
and NodeCons = SExpr * SExpr

and ObjExpr =
    | DotAccess of SExpr * Id
    | ObjInstan of TypeName * SExpr list

and SBinOp =
    | Pow | Times | Divide | Plus | Minus 
    | LessThan | LessThanEq | GreaterThan | GreaterThanEq
    | Equal | NotEqual
    | And | Or
    | DblColon
 
and SUnaryOp =
    | Not
    | Star

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
 
and NodeExpr = SExpr

and EdgeOp =
    // Edges can be weighted or unweighted
    | Right of SExpr Option
    | Left of SExpr Option
    | Bidir of SExpr Option

and TransformDef =
    Id * (string list) * (ExprStatement list) * MatchStatement

and MatchStatement =
    GExpr Option * MatchCase list

and MatchCase =
    MExpr * WhereClause Option * ExprStatement list * TerminatingStatement

and WhereClause = Expr list
    
and MExpr =
    | MExpr of GExpr
    | UnaryExpr of MUnaryOp * MExpr
    | BinExpr of MExpr * MBinOp * MExpr

and MBinOp =
    | And // Infix

and MUnaryOp =
    | Not // Prefix

and TerminatingStatement =
    | Return of Expr
    | Become of Expr

// Helpful type definitions to increase AST readability
and Id = string
and Param = string
and TypeName = string
and Attribute = string