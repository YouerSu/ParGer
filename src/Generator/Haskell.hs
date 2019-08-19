module Generator.Haskell (generate) where

import Generator.Rule
import Data.List (intercalate)
import System.IO

generate :: [Rule] -> String -> IO ()
generate rules parserName = do
  file <- openFile "parser.hs" WriteMode
  forEach rules (\x -> singleParser x file)
  hClose file

forEach [] _ = return ()
forEach (x:xs) action = do
  action x
  forEach xs action

dataPrefix = "GT" -- Stands for: Generate Type
funcPrefix = "p" -- Stands for: Parser
--need to improve
depends = "import Unit"
funcNameGen name order = name ++ (show order)
funcVar name = "\n  where\n    value = pick $ " ++ funcName 
  where
    funcName = funcNameGen name 1

changeFuncName :: String -> String -> Int -> Handle -> IO ()
changeFuncName funcName changeTo count handle = hPutStrLn handle ((funcNameGen funcName count) ++ " = " ++ (funcNameGen changeTo count))
--parser :: String -> Value (AST String,String)
whereGen = "\n  where"
varGen varName funcName paramName = "\n    " ++ varName ++ " = " ++ funcName ++ (' ':paramName)
funcHead funcName count = (funcNameGen funcName count) ++ " list = "

singleParser :: Rule -> Handle -> IO ()
singleParser (Ass name body) handle = do
  hPutStrLn handle (funcName ++ " list = Value value tail  \"" ++ name ++ "\"" ++ whereGen ++ (varGen "(value,tail)" nextFuncName "list"))
  singleParserGen body funcName 1 handle
  where
    funcName = 'p':name
    nextFuncName = funcNameGen funcName 1

singleParserGen :: Rule -> String -> Int -> Handle -> IO ()
singleParserGen (RuleName name) funcName count handle = do
  hPutStrLn handle ((funcHead funcName count) ++ "(Combin value,tail)" ++ whereGen ++ (varGen  "(value,tail)" ruleName "list"))
  where
    ruleName = funcNameGen name 1

singleParserGen (Combinator rule nextRule) funcName count handle = do
  hPutStrLn handle ((funcHead funcName count) ++ "(value1 ++ value2,tail)" ++ whereGen ++ (varGen "(value1,last)" "firstFuncName" "list") ++ (varGen "(value2,tail)" "nextFuncName" "last"))
  singleParserGen rule firstRuleName count handle
  changeFuncName funcName nextFuncName nextCount handle
  singleParserGen rule nextRuleFuncName nextCount handle
  where
    nextCount = count + 1
    firstRuleName = funcName
    nextRuleFuncName = funcName ++ "next"
    firstFuncName = funcNameGen firstRuleName count
    nextFuncName = funcNameGen nextRuleFuncName nextCount

singleParserGen (Terminator value) funcName count handle = do
  hPutStrLn handle ((funcHead funcName count) ++ "let (value,tail) = divide " ++ value ++ " list in (value:[],tail)")
--(funcHead funcName count) ++ "(value `bind` values,tail)" ++ whereGen ++ (varGen "(value,l)" nextFuncName "list") ++ (varGen "(values,tail)" nextFuncName "l")
--  where
--    nextFuncName = funcName name (count+1)
    
singleParserGen (Cycle rule) funcName count handle = do
  changeFuncName funcName thisFuncName count handle
  hPutStrLn handle ((funcNameGen thisFuncName count) ++ " list = ruleCycle " ++ ruleFuncName ++ " list")
  where
    thisFuncName = funcName ++ "Cycle"
    ruleFuncName = funcNameGen funcName (count+1)
    
singleParserGen (Cond rule) funcName count handle = do
  changeFuncName funcName thisFuncName count handle
  hPutStrLn handle ((funcNameGen thisFuncName count) ++ " list = ruleCond " ++ ruleFuncName ++ " list")
  where
    thisFuncName = funcName ++ "Cond"
    ruleFuncName = funcNameGen funcName (count+1)

singleParserGen (Or rulel ruler) funcName count handle = do
  changeFuncName funcName leftRuleName count handle
  changeFuncName funcName rightRuleName nextCount handle
  hPutStrLn handle ((funcHead funcName count) ++ "ruleOr " ++ leftFuncName ++ " " ++ rightFuncName ++ " list")
  where
    nextCount = count + 1
    leftRuleName = funcName ++ "LeftOr"
    rightRuleName = funcName ++ "RightOr"
    leftFuncName = funcNameGen leftRuleName count
    rightFuncName = funcNameGen rightRuleName count
