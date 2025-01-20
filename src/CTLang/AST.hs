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
  deriving (Show, Eq)
  
data Function = IHaveNoIdeaWhatWouldGoHere
  deriving (Show, Eq)

data SExpr
  = Cons SExpr SExpr
  | Atom Atom
  deriving (Show, Eq)

newtype Env = Env (M.Map String Atom)

data BuiltIn
  = Eval
  | If
  deriving (Show, Eq)

bareEnv :: Env
bareEnv = Env $ M.fromList
  [("eval", BuiltIn Eval)
  , ("if", BuiltIn If)
  ]

uncons :: SExpr -> (SExpr, SExpr)
uncons s = case s of
  Cons x y -> (x, y)
  _ -> error "Cannot uncons if it's not cons" -- TODO: Return Either
