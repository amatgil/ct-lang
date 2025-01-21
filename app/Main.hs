module Main where

import CTLang.AST
import CTLang.Eval

evaled = eval (Atom (Int 7)) $ bareEnv

main :: IO ()
main = print evaled
  -- putStrLn "Hello, Haskell!"
