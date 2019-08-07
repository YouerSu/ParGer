module Generator.Rule where

type Name = String
data Rule =
  Ass Name Rule|
  Combinator Rule Rule|
  Or Rule Rule|
  Cycle Rule|
  Cond Rule|
  RuleName Name|
  Terminator String|
  Empty
  deriving (Show)
