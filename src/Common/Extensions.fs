module Bilbo.Common.Extensions

let (|->) l r = Result.bind (r) l
let (|=>) l r = Result.bind (r >> Ok) l