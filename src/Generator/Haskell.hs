module Generator.Haskell (generate) where

import Generator.Rule
import Data.List (intercalate)

generate :: [Rule] -> String -> IO ()
generate rules parserName = print

dataPrefix = "GT" -- Stands for: Generate Type
funcPrefix = "p" -- Stands for: Parser
-- default data
dataOr = "data Or a b = LOR a|ROR b"
dataCond = "data Cond a = Cond a|Empty"
  
--dataGenerate =
generateData rules = "data Exp = " ++ $ intercalate "\n\t|"  (map rules singleData)
singleData (Ass name body) = (dataPrefix++name) ++ (singleData body)
singleData (RuleName name) = "Exp"
singleData (Terminator name) = "String"
singleData (Cycle rule) = '[':(singleData rule) ++ "]"
singleData (Cond rule) = "(Cond "++ (singleData rule) ++ ")"
singleData (Combinator rule nextRule) = (singleData rule) ++ " " ++ (singleData nextRule)
singleData (Or rulel ruler) = "(OR " ++ (singleData rulel) ++ " " ++ (singleData ruler) ++ ")"

--need to improve
depends = "import Unit"
funcNameGen name order = p:(name ++ (show order))

singleParser :: Rule -> Int -> String
singleParser (Ass name body) = ('p':name) ++ " = " ++ nextFuncName
  where
    nextFuncName = funcNameGen name 1
  
singleParser (RuleName name) = '(' : (dataPrefix ++ name ++ ")")
singleParser (Terminator name) = "String"
singleParser (Cycle rule) = '[':(singleParser rule) ++ "]"
singleParser (Cond rule) = "(Cond "++ (singleParser rule) ++ ")"
singleParser (Combinator rule nextRule) = (singleParser rule) ++ (' ' : (singleParser nextRule))
singleParser (Or rulel ruler) = "(OR " ++ (singleParser rulel) ++ " " ++ (singleParser ruler) ++ ")"
