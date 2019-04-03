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
    | Var   of Id
    // Simple expressions
    | SBinExpr of SBinExpr
    | SPrefixExpr of SPreOp * Expr
    | ObjExpr of ObjExpr
    | Literal of Literal
    // Graph expressions
    | PathExpr of PathExpr
    | GBinExpr of Expr * GBinOp * Expr
    // Match expressions
    | MPrefixExpr of MPreOp * Expr
    | MBinExpr of Expr * MBinOp * Expr

and SBinExpr = Expr * SBinOp * Expr 
 
and NodeCons = Expr * Expr

and ObjExpr =
    | DotAccess of Expr * Id
    | ObjInstan of TypeName * Expr list

and SBinOp =
    | Pow | Times | Divide | Plus | Minus 
    | LessThan | LessThanEq | GreaterThan | GreaterThanEq
    | Equal | NotEqual
    | And | Or
    | NodeCons
 
and SPreOp =
    | Not
    | Star

and Literal =
    | StrLit of string
    | FloatLit of float
    | IntLit of int
    | BoolLit of bool
      
and GBinOp =
    | Plus
    | Minus

and PathExpr =
    | Path of PathElem list

and PathElem =
    | Node of Expr
    | Edge of Expr * EdgeOp * Expr
 
and EdgeOp =
    // Edges can be weighted or unweighted
    | Right of Expr Option
    | Left of Expr Option
    | Bidir of Expr Option

and TransformDef =
    Id * (string list) * (ExprStatement list) * MatchStatement

and MatchStatement =
    Expr Option * MatchCase list

and MatchCase =
    Expr * WhereClause Option * ExprStatement list * TerminatingStatement 

and WhereClause = Expr list

and MBinOp =
    | And // Infix

and MPreOp =
    | Not // Prefix

and TerminatingStatement =
    | Return of Expr
    | Become of Expr

// Helpful type definitions to increase AST readability
and Id = string
and Param = string
and TypeName = string
and Attribute = string

// type AExpr =
//     | BinExpr of AExpr * ABinOp * AExpr
//     | PrefixExpr of APreOp * AExpr
//     | PostficExpr of AExpr * APostOp

// and ABinOp =
//     | Pipe
//     | ChoicePipe

// and APreOp =
//     | Dollar
 
// and APostOp =
//     // `**
//     | DblTimes
//     // As-long-as-possible application `!`
//     | ALAPApp


