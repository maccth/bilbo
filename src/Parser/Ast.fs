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
    // Having an Expr on the LHS of assignement makes parsing easier.
    //  In particular, one need not distinguish between field access
    //  and field assignment
    | AssignmentExpr of Expr * Expr
    
and Expr =
    | VExpr of VExpr
    | SExpr of SExpr
    | GExpr of GExpr
    // | MExpr of MExpr
    | AExpr of AExpr
    | TExpr of TExpr
    | NodeCons of NodeCons
    | BinExpr of Expr * BinOp * Expr
    | PrefixExpr of PreOp  * Expr
    | PostfixExpr of Expr * PostOp

and VExpr =
    | Var of Id
    | DotAccess of Expr * Id

and SExpr =
    | ObjExpr of ObjExpr
    | Literal of Literal
    // | SBinExpr of SBinExpr
    // | SPrefixExpr of SPreOp * Expr
    // TODO: add param lists for (a,b,c) |> t

and GExpr =
    | PathExpr of PathExpr
    // | GBinExpr of Expr * GBinOp * Expr

// and MExpr =
    // | MPrefixExpr of MPreOp * Expr
    // | MBinExpr of Expr * MBinOp * Expr

and NodeCons = Expr * Expr

// and SBinExpr = Expr * SBinOp * Expr 
 
and ObjExpr =
    | ObjInstan of TypeName * Expr list

and BinOp =
    // Simple
    // ======
    // Originally simple, when life was complicated...
    //  and I was trying to parse-time semantic analysis
    | Pow | Times | Divide | Plus | Minus 
    | LessThan | LessThanEq | GreaterThan | GreaterThanEq
    | Equal | NotEqual
    | Is
    | And | Or
    // | Dot

    // Application
    // ===========
    | Pipe
    | OrPipe

    // Transform
    // ===========
    // **, *!*
    | MulApp 
    | UpToApp

    // Graph
    // =====
    // | GAdd
    // | GSub

    // Match
    // =====
    // | And

and PreOp =
    // Simple
    // ======
    | Not
    | Amp

    // Transform
    // =========
    | Dollar

    // Match
    // =====
    // | Not

and PostOp =
    // Transform
    // =========
    | ALAPApp
    | MaybeApp





and Literal =
    | StrLit of string
    | FloatLit of float
    | IntLit of int
    | BoolLit of bool
     
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
    Id * (Param list) * (ExprStatement list) * MatchStatement

and MatchStatement =
    Expr Option * MatchCase list

and MatchCase =
    Expr * WhereClause Option * ExprStatement list * TerminatingStatement 

and WhereClause = Expr list

and TerminatingStatement =
    | Return of Expr
    | Become of Expr

and TExpr =
    | TTerm of TTerm
    // | TBinExpr of Expr * TBinOp * Expr
    // | TPrefixExpr of TPreOp * Expr
    // | TPostfixExpr of Expr * TPostOp

// Extend this for PExprs {a,b,c...}
and TTerm = Id

and AExpr =
    | ATerm of Expr
    // | ABinExpr of Expr * ABinOp * Expr

// Helpful type definitions to increase AST readability
and Id = string
and Param = string
and TypeName = string
and Attribute = string