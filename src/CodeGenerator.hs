module CodeGenerator where

import Generator.Rule
import qualified Generator.Haskell as Haskell

generate :: [Rule] -> IO ()
generate ((Ass "lang" (RuleName "haskell")):rules) = Haskell.generate rules
generate ((Ass "lang" (RuleName name)):_) = error $ "UnDefined lang:" ++ name
