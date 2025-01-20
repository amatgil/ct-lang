module CTLang.AST where

data Atom 
  = String String  -- "Haii"
  | Int Int        -- 42
  | Bool Bool      -- #true, #false
  | Float Float    -- 1.2
  | Keyword String -- :foo
  | Symbol String  -- something
  
data SExpr = List [SExpr] | Atom Atom


  
