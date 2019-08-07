module RuleParser where

import qualified RuleLexer as Lexer
import Generator.Rule

parse :: [Lexer.Exp] -> Rule
parse ((Lexer.ExpName name):(Lexer.Ass):value) = Ass name (combin Empty value)
combin :: Rule -> [Lexer.Exp] -> Rule --combin the unary operator first
combin rule [] = rule
combin Empty ((Lexer.ExpName name):next) = combin (RuleName name) next
combin Empty ((Lexer.Terminator name):next) = combin (Terminator name) next
combin Empty ((Lexer.CycleStart):body) = combin Empty body
combin rule ((Lexer.CycleEnd):value) = combin (Cycle rule) value
combin Empty ((Lexer.CondStart):body) = combin Empty body
combin rule ((Lexer.CondEnd):value) = combin (Cond rule) value
-- Binary operator
combin rule ((Lexer.Or):value) = combin (Or rule nextValue) nextBody
  where
    all = nextRule value
    nextValue = fst all
    nextBody = snd all

combin rule ((Lexer.Cable):value) = combin (Combinator rule nextValue) nextBody
  where
    all = nextRule value
    nextValue = fst all
    nextBody = snd all
    
nextRule :: [Lexer.Exp] -> (Rule,[Lexer.Exp])
nextRule ((Lexer.ExpName name):next) = ((RuleName name),next)
nextRule ((Lexer.Terminator name):next) = ((Terminator name),next)
nextRule (Lexer.CycleStart:body) = getCycleBody body [] 1
nextRule (Lexer.CondStart:body) = getCondBody body [] 1

getCycleBody :: [Lexer.Exp] -> [Lexer.Exp] -> Int -> (Rule,[Lexer.Exp])
getCycleBody (Lexer.CycleEnd:xs) body 1 = ((combin Empty body),xs )
getCycleBody (Lexer.CycleEnd:xs) body count = getCycleBody xs (body ++ [Lexer.CycleStart]) (count - 1)
getCycleBody (Lexer.CycleStart:xs) body count = getCycleBody xs (body ++ [Lexer.CycleStart]) (count + 1)
getCycleBody (x:xs) body count = getCycleBody xs (body ++ [x]) count
getCondBody :: [Lexer.Exp] -> [Lexer.Exp] -> Int -> (Rule,[Lexer.Exp])
getCondBody (Lexer.CondEnd:xs) body 1 = ((combin Empty body),xs )
getCondBody (Lexer.CondEnd:xs) body count = getCondBody xs (body ++ [Lexer.CondStart]) (count - 1)
getCondBody (Lexer.CondStart:xs) body count = getCondBody xs (body ++ [Lexer.CondStart]) (count + 1)
getCondBody (x:xs) body count = getCondBody xs (body ++ [x]) count
