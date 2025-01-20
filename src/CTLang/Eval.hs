module Eval where
import CTLang.AST 

eval :: SExpr -> Env -> Atom
eval input = case input of
  Atom a -> a
  List [] -> error "Cannot evaluate empty list" -- TODO: return Either
  List (e : es) ->
    let f = e
     in case eval e env of
          Symbol s -> undefined -- call function `s` with argument list `es`
          x -> error "Can not call as function"
    
  
  
