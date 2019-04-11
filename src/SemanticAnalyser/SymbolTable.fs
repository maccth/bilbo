module Bilbo.SemanticAnalyser.SymbolTable

open Bilbo.Common.Ast
open Bilbo.SemanticAnalyser.Value

type SymbolTable = {
    types       : Map<string * string,  TypeDef>; 
    transforms  : Map<string * string,  TransformDef>;
    variables   : Map<string * string,  Value>;
    namespaces  : Map<string,           string>;
}

let emptySymbolTable = {
    types       = Map.empty;
    transforms  = Map.empty;
    variables   = Map.empty;
    namespaces  = Map.empty;
}

let addType (st : SymbolTable) (nspace,tname) tdef =
    {st with types = Map.add (nspace,tname) tdef st.types}

let addTransform (st : SymbolTable) (nspace,tname) tdef =
    {st with transforms = Map.add (nspace,tname) tdef st.transforms}

let addNamespace (st : SymbolTable) nspace fp =
    {st with namespaces = Map.add nspace fp st.namespaces}