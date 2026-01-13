module JoinList where

import Data.Monoid
import Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: (Monoid m) => JoinList m a -> m
tag (Append m _ _) = m
tag (Single m _) = m
tag Empty = mempty

(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty Empty = Empty
(+++) x Empty = x
(+++) Empty y = y
(+++) x y = Append res x y
  where
    res = tag x <> tag y

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x : xs) !!? 0 = Just x
(x : xs) !!? i = xs !!? (i - 1)

len ::
  (Sized b, Monoid b) =>
  JoinList b a -> Int
len = getSize . size . tag

indexJ ::
  (Sized b, Monoid b) =>
  Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ i jl@(Append s l r)
  | i < len l = indexJ i l
  | i < len jl = indexJ (i - len l) r
  | otherwise = Nothing
indexJ _ _ = Nothing

dropJ ::
  (Sized b, Monoid b) =>
  Int -> JoinList b a -> JoinList b a
dropJ n jl
  | n <= 0 = jl
  | otherwise = case jl of
      (Append {}) -> f jl
      _ -> Empty
  where
    f jl@(Append s l r)
      | n < len l = dropJ n l
      | n < len jl = dropJ (n - len l) r
      | otherwise = Empty

takeJ ::
  (Sized b, Monoid b) =>
  Int -> JoinList b a -> JoinList b a
takeJ n jl
  | n <= 0 = Empty
  | otherwise = case jl of
      (Append {}) -> f jl
      _ -> jl
  where
    f jl@(Append s l r)
      | n < len l = takeJ n l
      | n < len jl = (+++) l $ takeJ (n - len l) r
      | otherwise = jl
