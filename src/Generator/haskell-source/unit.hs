module Unit where

import Data.List

data AST a = Node a|Combin [(AST a)]|Error

instance Functor AST where
  fmap f (Node value) = pure $ f value

instance Applicative AST where
  pure a = Node a
  (Node f) <*> (Node value) = pure $ f value

instance Monad AST where
  (Node value) >>= f = f value
  
pick (Node value) = value

divide goal resource
  |bool = ((Node goal),tail)
  |otherwise =(Node [],resource)
   where
     pair = check goal resource
     bool = fst pair
     tail = snd pair

check :: String -> String -> (Bool,String)
check [] tail = (True,tail)
check _ [] = (False,[])
check (x:xs) (y:ys)
  |x == y = check xs ys
  |otherwise = (False,[])

bind :: AST String -> [AST String] -> [AST String]
(Node []) `bind` _ = []
value `bind` list = do
  value:list
