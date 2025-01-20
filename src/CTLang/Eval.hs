module CTLang.Eval where
import CTLang.AST 
import qualified Data.Map as M

eval :: SExpr -> Env -> (Atom, Env)
eval input env = case input of
  Atom a -> (a, env)
  ConsCell left right ->
    case l of
      BuiltIn b -> evalBuiltIn b env right
      Symbol s -> undefined -- call s with `right` as arguments
      _ -> error "Tried to evaluate cons that wasn't a symbol or a builtin"
    where
      (l, env') = eval left env

apply :: Null
apply = undefined

evalBuiltIn :: BuiltIn -> Env -> SExpr -> (Atom, Env)
evalBuiltIn builtIn env sexpr = case builtIn of
  Eval -> eval sexpr env
  If ->
    let (cond, rest) = uncons sexpr -- TODO: Make this Either and do notation
        (evaledCond, env') = eval cond env
        (whenTrue, rest') = uncons rest
        (whenFalse, rest'') = uncons rest'
     in if rest'' /= Atom Nil
          then error "Too many arguments to if expr"
          else
            if evaledCond == Bool True
              then eval whenTrue env'
              else
                if evaledCond == Bool False
                  then eval whenFalse env'
                  else error "Condition must be boolean in if"

evalDefine :: String -> Atom -> Env -> Env
evalDefine = M.insert
