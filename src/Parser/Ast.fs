module Bilbo.Parser.Ast

type Program = Declaration list

and Declaration =
    | TypeDeclaration of TypeName * Attribute list
    // TODO: add inheritance types

and TypeName = string
and Attribute = string 