module CTLang.Eval where
import CTLang.AST 

eval :: SExpr -> Env -> (Atom, Env)
eval input env = case input of
  Atom a -> (a, env)
  Cons left right ->
    let (l, env') = eval left env
     in case l of
          Symbol s -> undefined -- call s with `right` as arguments
          _ -> error "Tried to evaluate cons that wasn't a symbol"

