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
    // TODO: These types will allow for better domain modelling in the ASG
    // | MExpr of MExpr
    // | AExpr of AExpr
    // | TExpr of TExpr
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
    // TODO: add param lists for (a,b,c) |> t

and GExpr =
    | PathExpr of PathExpr

// and MExpr =
    // | MPrefixExpr of MPreOp * Expr
    // | MBinExpr of Expr * MBinOp * Expr

and NodeCons = Expr * Expr
 
and ObjExpr =
    | ObjInstan of TypeName * Expr list

and BinOp =
    // Simple
    | Pow | Times | Divide | Plus | Minus 
    | LessThan | LessThanEq | GreaterThan | GreaterThanEq
    | Equal | NotEqual
    | And | Or
    | Is
    // | Dot
    // Application
    | Pipe | OrPipe
    // Transform
    | MulApp | UpToApp
    
    // Graph
    // | GAdd
    // | GSub

    // Match
    // | And

and PreOp =
    // Simple
    | Not | Amp
    // Transform
    | Dollar
    // Match
    // | Not

and PostOp =
    // Transform
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

// Helpful type definitions to increase AST readability
and Id = string
and Param = string
and TypeName = string
and Attribute = string