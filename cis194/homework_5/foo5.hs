{-# LANGUAGE TypeSynonymInstances #-}

import Parser
import StackVM

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

class ExprB a where
  litB :: Bool -> a
  or :: a -> a -> a
  and :: a -> a -> a

instance Expr Program where
  lit = (: []) . PushI
  add x y = x ++ y ++ [Add]
  mul x y = x ++ y ++ [Mul]

instance ExprB Program where
  litB = (: []) . PushB
  or x y = x ++ y ++ [Or]
  and x y = x ++ y ++ [And]

compile :: String -> Maybe Program
compile = parseExp lit add mul
