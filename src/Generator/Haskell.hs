module Generator.Haskell (generate) where

import Generator.Rule
import Data.List (intercalate)
import System.IO

moduleName = "module Parser where\n"
depends = "import Unit\n"

generate :: [Rule] -> IO ()
generate rules = do
  file <- openFile "Parser.hs" WriteMode
  hPutStrLn file moduleName
  hPutStrLn file depends
  forEach rules (\x -> singleParser x file)
  hClose file

forEach [] _ = return ()
forEach (x:xs) action = do
  action x
  forEach xs action

whereGen = "\n  where"
varGen varName funcName paramName = "\n    " ++ varName ++ " = " ++ funcName ++ (' ':paramName)
funcHead funcName = funcName ++ " list = "

--parser :: String -> Value (AST String,String)
singleParser :: Rule -> Handle -> IO ()
singleParser (Ass name body) handle = do
  hPutStrLn handle (funcName ++ " list = Value value tail  \"" ++ name ++ "\"" ++ whereGen ++ (varGen "(value,tail)" nextFuncName "list"))
  singleParserGen body nextFuncName handle
  where
    funcName = 'b':name
    nextFuncName = 'p':name

singleParserGen :: Rule -> String -> Handle -> IO ()
singleParserGen (RuleName name) funcName handle = do
  hPutStrLn handle ((funcHead funcName) ++ "((Combin value):[],tail)" ++ whereGen ++ (varGen  "(value,tail)" ruleName "list"))
  where
    ruleName = 'p':name

singleParserGen (Combinator rule nextRule) funcName handle = do
  hPutStrLn handle ((funcHead funcName) ++ "((value1 ++ value2),tail)" ++ whereGen ++ (varGen "(value1,last)" firstRuleName "list") ++ (varGen "(value2,tail)" nextRuleName "last"))
  singleParserGen rule firstRuleName handle
  singleParserGen nextRule nextRuleName handle
  where
    firstRuleName = funcName ++ "First"
    nextRuleName = funcName ++ "Next"

singleParserGen (Terminator value) funcName handle = do
  hPutStrLn handle ((funcHead funcName) ++ "let (value,tail) = divide \"" ++ value ++ "\" list in (value:[],tail)")
    
singleParserGen (Cycle rule) funcName handle = do
  hPutStrLn handle ((funcHead funcName) ++ "let (rule,tail) = ruleCycle " ++ ruleName ++ " list in (rule:[],tail)")
  singleParserGen rule ruleName handle
  where
    ruleName = funcName ++ "Cycle"
    
singleParserGen (Cond rule) funcName handle = do
    hPutStrLn handle ((funcHead funcName) ++ "let (rule,tail) = ruleCond " ++ ruleName ++ " list in (rule:[],tail)")
    singleParserGen rule ruleName handle
  where
    ruleName = funcName ++ "Cond"

singleParserGen (Or rulel ruler) funcName handle = do
  hPutStrLn handle ((funcHead funcName) ++ "ruleOr " ++ leftRuleName ++ " " ++ rightRuleName ++ " list")
  singleParserGen rulel leftRuleName handle
  singleParserGen ruler rightRuleName handle
  where
    leftRuleName = funcName ++ "LeftOr"
    rightRuleName = funcName ++ "RightOr"
