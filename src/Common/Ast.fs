module Bilbo.Common.Ast

type Program = ProgramUnit list

and ProgramUnit = SpaceId list * Statement

and SpaceId =
    | Top
    | Name of Id

and Loc = {
        file        : FilePath
        startLine   : int64
        startCol    : int64
        endLine     : int64
        endCol      : int64
    } 

and Statement =
    | TypeDefL of Loc * TypeDef
    | TransformDefL of Loc * TransformDef
    | FunctionDefL of Loc * FunctionDef
    | ExprStatementL of ExprStatementL
    | ImportL of Loc * Import

and Import =
    | ImportTo of FilePath
    | ImportAs of FilePath * string
    
and TypeDef = TypeName * Attribute list
    
and ExprStatementL = Loc * ExprStatement

and ExprStatement =
    | AssignmentExpr of Expr * Expr
    | PrintExpr of Expr * Expr Option
    | DeleteExpr of Id
    
and Expr =
    | Var of Id
    | SExpr of SExpr
    | GExpr of GExpr
    | BinExpr of Expr * BinOp * Expr
    | PrefixExpr of PreOp  * Expr
    | PostfixExpr of Expr * PostOp
    | SpecialExpr of SpecialExpr

and SExpr =
    | ObjExpr of ObjExpr
    | TypeCast of TypeCast
    | Literal of Literal
    | ParamList of Expr list

and GExpr =
    | PathExpr of PathExpr

and TypeCast = PrimTypeName * Expr

and ObjExpr =
    | ObjInstan of TypeName * Expr list
    | ParamObjInstan of TypeName * (Attribute * Expr) list

and BinOp =
    | NodeCons
    | Dot
    | Pow | Times | Divide | Plus | Minus | Percent
    | LessThan | LessThanEq | GreaterThan | GreaterThanEq
    | Equal | NotEqual
    | And | Or | Xor
    | Is | Has
    | ThenPipe | OrPipe | AndPipe
    | Enpipe
    | MulApp
    | Collect

and PreOp =
    | Not
    | Amp | DblAmp
    | Dollar

and PostOp =
    | AlapApp
    | MaybeApp

and SpecialExpr =
    | PosPatternGraph

and Literal =
    | StrLit of string
    | FloatLit of float
    | IntLit of int
    | BoolLit of bool

and PrimTypeName =
    | CastInt
    | CastFloat
    | CastBool
    | CastString
     
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

and TransformDef = Id * (Param list) * (ExprStatementL list) * MatchStatement
// TODO: Implicit (point-free / unix-style) transforms. Not for this version of Bilbo. Maybe v2.
// Code sketch of types...
// and TransformDef =   
//     | ExplicitTransform of Id * (Param list) * (ExprStatementL list) * ExplicitMatch
//     | ImplicitTransform of Id * (ExprStatementL list) * ImplicitMatch
// 
// and ImplicitMatch = MatchCase list
// and ExplicitMatch = Expr * MatchCase list

and FunctionDef =
    Id * (Param list) * (ExprStatementL list) * Expr

and MatchStatement = Expr * MatchCase list

and MatchCase =
    | Pattern of PGraphExpr * PGraphExpr option * WhereClause Option * ExprStatementL list * TerminatingStatement
    | CatchAll of WhereClause Option * ExprStatementL list * TerminatingStatement

and PGraphExpr =
    | PGraph of PathExpr
    | PGraphBinExpr of PGraphExpr * PGraphBinOp * PGraphExpr
    // | PGraphPreExpr of PGraphPreOp * PGraphExpr

and PGraphBinOp = 
    | PGAdd
    | PGSub
    | PGAnd
    | PGOr

// and PGraphPreOp =
    // | PGNot
    
and WhereClause = Expr

and TerminatingStatement =
    | Return of Expr
    | Become of Expr

// Helpful type definitions to increase AST readability
and Id = string
and Param = string
and TypeName = string
and Attribute = string
and FilePath = string