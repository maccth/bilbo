module Bilbo.Parser.Ast

type Program = ProgramUnit list

and ProgramUnit =
    | Statement of Statement

and Statement =
    | TypeDef of TypeDef
    | TransformDef of TransformDef
    | ExprStatement of ExprStatement
    | Import of FilePath * string

and TypeDef = TypeName * Attribute list
    
and ExprStatement =
    | AssignmentExpr of Expr * Expr
    | PrintExpr of Expr * Expr Option
    
and Expr =
    | Var of Id
    | SExpr of SExpr
    | GExpr of GExpr
    | BinExpr of Expr * BinOp * Expr
    | PrefixExpr of PreOp  * Expr
    | PostfixExpr of Expr * PostOp

and SExpr =
    | ObjExpr of ObjExpr
    | Literal of Literal
    | ParamList of Expr list

and GExpr =
    | PathExpr of PathExpr

 
and ObjExpr =
    | ObjInstan of TypeName * Expr list

and BinOp =
    | NodeCons
    | Dot
    // Simple
    | Pow | Times | Divide | Plus | Minus 
    | LessThan | LessThanEq | GreaterThan | GreaterThanEq
    | Equal | NotEqual
    | And | Or
    | Is
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
    | Not | Amp | DblAmp
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
    | PathComp of PathCompOp list * PathElem list

and PathElem =
    | Node of Expr
    | Edge of Expr * EdgeOp * Expr

and PathCompOp =
    | AddEdge of EdgeOp
    // &&=expr
    | SetLoad of Expr
    // &=
    | SetId
 
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
    | Case of Expr * WhereClause Option * ExprStatement list * TerminatingStatement
    | CatchAll of WhereClause Option * ExprStatement list * TerminatingStatement

and WhereClause = Expr list

and TerminatingStatement =
    | Return of Expr
    | Become of Expr

// Helpful type definitions to increase AST readability
and Id = string
and Param = string
and TypeName = string
and Attribute = string
and FilePath = string