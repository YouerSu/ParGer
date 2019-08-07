module RuleLexer where

import Data.List.Split

type Name = String
data Exp =
  --data--
  ExpName Name|
  Terminator Name|
  --operator--
  Ass|Cable|Or|CycleStart|CycleEnd|CondStart|CondEnd
  deriving (Show)

ass = "::="
cable = "+"
or = "|"
cyclel = "{"
cycler = "}"
condl =  "["
condr = "]"
seperateToken = "+|{}[]"
upperLetter = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
lowerLetter = "abcdefghizklmnopqrstuvwxyz"
name = upperLetter ++ lowerLetter
blanks = "\t\n\r"
delBlanks [] = []
delBlanks (x:xs)
  |elem x blanks = delBlanks xs
  |otherwise = x:delBlanks xs


getExps :: String -> [String]
getExps str = map delBlanks $ wordsBy (== ';') str
tokenStream :: String -> [String]
tokenStream = seperateExps . words
expAnalysis :: String -> [Exp]
expAnalysis str = map match $ tokenStream str
expStream :: String -> [[Exp]]
expStream = (map expAnalysis) . getExps

seperateExps :: [String] -> [String]
seperateExps [] = []
seperateExps (x:xs)
  |foldr (\elemt bool -> (elem elemt x)||bool)  False seperateToken = (seperate x []) ++ seperateExps xs
  |otherwise = x : (seperateExps xs)

seperate :: String -> [String] -> [String]
seperate (x:xs) acc@(y:ys)
  |elem x seperateToken = (seperate [] ((x:[]):acc)) ++ (seperate xs [])
  |otherwise = seperate xs ((y ++ (x:[])):ys)
seperate (x:xs) []
  |elem x seperateToken = (x:[]):seperate xs []
  |otherwise = seperate xs ((x:[]):[])
seperate [] acc = foldl (\list x -> x:list) [] acc

match :: String -> Exp
match "::=" = Ass
match "+" = Cable
match "|" = Or
match "{" = CycleStart
match "}" = CycleEnd
match "[" = CondStart
match "]" = CondEnd
match ('\'':symbol) = Terminator symbol
match ruleName
  |and $ map (\x -> elem x name) ruleName = ExpName ruleName
  |otherwise = error $ "Lexer:Can't analysis the str" ++ ruleName
