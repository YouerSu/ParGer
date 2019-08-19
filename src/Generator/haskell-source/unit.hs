module Unit where

--import Data.List

data AST a = Node a|Combin [(AST a)]|Cycle [AST a]|Cond [AST a]|Error
  deriving (Eq)

instance Functor AST where
  fmap f (Node value) = pure $ f value

instance Applicative AST where
  pure a = Node a
  (Node f) <*> (Node value) = pure $ f value

instance Monad AST where
  (Node value) >>= f = f value

data Value v = Value {
                        total :: v,
                        tail :: String,
                        gt :: String
                        }

instance Functor Value where
  fmap f (Value v tail gt) = Value (f v) tail gt

instance Applicative Value where
  pure v = Value v [] []
  (Value f [] gt) <*> (Value v t _) = Value (f v) t gt

instance Monad Value where
  (Value v t gt) >>= f
    |t /= t' = value
    |otherwise = Value v' t gt
    where
      value@(Value v' t' _) = f v

pick (Node value) = value

divide goal resource
  |bool = ((Node goal),tail)
  |otherwise =(Error,resource)
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

infixl 8 `bind`
bind :: AST String -> [AST String] -> [AST String]
Error `bind` _ = [Error]
_ `bind` [Error] = [Error]
value `bind` list = value:list

infixl 8 `uor`
uor :: [AST String] -> [AST String] -> [AST String]
[Error] `uor` [Error] = [Error]
[Error] `uor` value = value `uor` [Error] --Fuor >>= run cuorrect
value `uor` _ = value

ruleCycle rulePar list = (Cycle value,tail)
  where
    (value,tail) = cycleRule rulePar list
cycleRule rulePar list
  |v == [Error] = ([],list)
  |otherwise = let (value,tail) = (cycleRule rulePar t) in (v++value,tail)
  where
    (v,t) = rulePar list

ruleCond rulePar list
  |v == [Error] = (Cond [],list)
  |otherwise = (Cond v,t)
  where
    (v,t) = rulePar list

ruleOr lRulePar rRulePar list
  |value /= [Error] = left
  |otherwise = right
  where
    left@(value,_) = lRulePar list
    right = rRulePar list
