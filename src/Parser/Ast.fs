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
    | Var of Id
    | SExpr of SExpr
    | GExpr of GExpr
    | MExpr of MExpr
    // | AExpr of AExpr
    | TExpr of TExpr
    | NodeCons of NodeCons

and SExpr =
    | SBinExpr of SBinExpr
    | SPrefixExpr of SPreOp * Expr
    | ObjExpr of ObjExpr
    | Literal of Literal

and GExpr =
    | PathExpr of PathExpr
    | GBinExpr of Expr * GBinOp * Expr

and MExpr =
    | MPrefixExpr of MPreOp * Expr
    | MBinExpr of Expr * MBinOp * Expr

and NodeCons = Expr * Expr

and SBinExpr = Expr * SBinOp * Expr 
 
and ObjExpr =
    | DotAccess of Expr * Id
    | ObjInstan of TypeName * Expr list

and SBinOp =
    | Pow | Times | Divide | Plus | Minus 
    | LessThan | LessThanEq | GreaterThan | GreaterThanEq
    | Equal | NotEqual
    | Is
    | And | Or

and SPreOp =
    | Not
    | Amp

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
    | And

and MPreOp =
    | Not

and TerminatingStatement =
    | Return of Expr
    | Become of Expr

and TExpr =
    | TTerm of Expr
    | TBinExpr of Expr * TBinOp * Expr
    | TPrefixExpr of TPreOp * Expr
    | TPostfixExpr of Expr * TPostOp

and TBinOp =
    // Multiple application  `**`
    | MulApp 
    // Up-to application `*!*`
    | UpToApp

and TPostOp =
    // As-long-as-possible application `!`
    | ALAPApp
    // Maybe application `?`
    | MaybeApp

and TPreOp =
    | Dollar

// and APreOp =
//     | Dollar
 
// and APostOp =
//     // As-long-as-possible application `!`
//     | ALAPApp
//     // Maybe application `?`
//     | MaybeApp

// and AExpr =
//     | ATerm of ATerm
//     | ABinExpr of ATerm * ABinOp * ATerm

// and ATerm = (APreOp list) Option * Expr * (APostOp list) Option

// and ABinOp =
//     | Pipe
//     | OrPipe

// and APreOp =
//     | Dollar
 
// and APostOp =
//     // As-long-as-possible application `!`
//     | ALAPApp
//     // Maybe application `?`
//     | MaybeApp
//     // Multiple application  `** 5`
//     | MulApp of Expr
//     // Up-to application `*!* 7`
//     | UpToApp of Expr

// Helpful type definitions to increase AST readability
and Id = string
and Param = string
and TypeName = string
and Attribute = string