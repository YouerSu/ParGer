module CodeGenerator where

import Generator.Rule
import qualified Generator.Haskell as Haskell
import System.Directory
import Paths_ParGer

generate :: [Rule] -> IO ()
generate ((Ass "lang" (RuleName "haskell")):rules) = do
  filePath <- getDataFileName "source/haskell-source/Unit.hs"
  toPath <- getCurrentDirectory
  copyFile (filePath) (toPath ++"/Unit.hs")
  Haskell.generate rules
  
generate ((Ass "lang" (RuleName name)):_) = error $ "UnDefined lang:" ++ name
