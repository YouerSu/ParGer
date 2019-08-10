module Generator.Haskell (generate) where

import Generator.Rule
import Data.List (intercalate)

generate :: [Rule] -> String -> IO ()
generate rules parserName = print

dataPrefix = "GT" -- Stands for: Generate Type
funcPrefix = "p" -- Stands for: Parser
--need to improve
depends = "import Unit"
funcNameGen name order = p:(name ++ (show order))
funcVar name = "\n  where\n    value = pick $ " ++ funcName 
  where
    funcName = funcNameGen name 1

--parser :: String -> Value (AST String,String)
whereGen = "\n  where"
varGen varName funcName valueName = "\n    " ++ varName ++ " = " ++ funcName ++ (' ':valueName)
head funcName count = (funcNameGen funcName count) ++ " list = "
singleParser :: Rule -> Int -> String
singleParser (Ass name body) = ('p':name) ++ " list = Value value tail  " ++ name ++ whereGen ++ (varGen "(value,tail)" nextFuncName "list")
  where
    nextFuncName = funcNameGen name 1
  
singleParserGen (RuleName name) funcName count = (head funcName count) ++ "(Combin value `bind` values,tail)" ++ whereGen ++ (varGen  "(value,l)" ruleName "list") ++ (varGen "(values,tail)" nextFuncName "l")
  where
    ruleName = funcName name 1

singleParserGen (Terminator value) funcName count = (head funcName count) ++ "(value `bind` values,tail)" ++ whereGen ++ (varGen "(value,l)" nextFuncName "list") ++ (varGen "(values,tail)" nextFuncName "l")
  where
    nextFuncName = funcName name (count+1)
    
singleParserGen (Cycle rule) = '[':(singleParserGen rule) ++ "]"
singleParserGen (Cond rule) = "(Cond "++ (singleParserGen rule) ++ ")"
singleParserGen (Combinator rule nextRule) = (singleParserGen rule) ++ (' ' : (singleParserGen nextRule))
singleParserGen (Or rulel ruler) = "(OR " ++ (singleParserGen rulel) ++ " " ++ (singleParserGen ruler) ++ ")"
