module CTLang.AST where
import qualified Data.Map as M

data Atom 
  = String String  -- "Haii"
  | Int Int        -- 42
  | Bool Bool      -- #true, #false
  | Float Float    -- 1.2
  | Keyword String -- :foo
  | Symbol String  -- something
  | BuiltIn BuiltIn
  | Lambda Function
  | Nil
  
data Function = ??

data SExpr
  = Cons SExpr SExpr
  | Atom Atom

newtype Env = Env (M.Map String Atom)

data BuiltIn
  = Eval
  | Print

bareEnv :: Env
bareEnv = Env $ M.fromList [
  ("eval", BuiltIn Eval),
  ("print", BuiltIn Print)]

  
