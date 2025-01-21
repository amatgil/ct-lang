module CTLang.AST where

import qualified Data.Map as M

data Atom
  = String String -- "Haii"
  | Int Int -- 42
  | Bool Bool -- #true, #false
  | Float Float -- 1.2
  | Keyword String -- :foo
  | Symbol String -- something
  | BuiltIn BuiltIn
  | Clojure Clojure
  | Nil
  deriving (Show, Eq)

data Clojure = C
  deriving (Show, Eq)

data Function = IHaveNoIdeaWhatWouldGoHere
  deriving (Show, Eq)

data SExpr
  = ConsCell SExpr SExpr
  | Atom Atom
  deriving (Show, Eq)

newtype Env = Env (M.Map String Atom)
  deriving (Show)

-- Ideas: https://cs61a.org/articles/scheme-builtins/
data BuiltIn
  = Eval -- Half of the metacircular evaluator
  | Apply -- The other half of the metacircular evaluator
  | If -- Conditional Primitive
  | Car 
  | Cdr 
  | Cons
  | Quote
  | Define
  | Closure
  deriving (Show, Eq)

bareEnv :: Env
bareEnv = Env $ M.fromList
  [("eval", BuiltIn Eval)
  , ("if", BuiltIn If)
  , ("car", BuiltIn Car)
  , ("cdr", BuiltIn Cdr)
  , ("cons", BuiltIn Cons)
  , ("quote", BuiltIn Quote)
  , ("define", BuiltIn Define)
  --, ("lambda", Built)
  ]

lookup :: String -> Env -> Maybe Atom
lookup s (Env m) = M.lookup s m

uncons :: SExpr -> (SExpr, SExpr)
uncons s = case s of
  ConsCell x y -> (x, y)
  _ -> error "Cannot uncons if it's not cons cell" -- TODO: Return Either
