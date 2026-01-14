module JoinList where

import Buffer
import Data.Monoid
import Editor (editor, runEditor)
import Scrabble
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

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

type JLB = JoinList (Score, Size) String

instance Buffer JLB where
  toString Empty = mempty
  toString (Single _ s) = s
  toString (Append _ l r) = toString l ++ toString r
  fromString = foldr f (Empty :: JLB) . lines
    where
      f a b = single a +++ b
      single s = Single (scoreString s, Size 1) s
  line = indexJ
  replaceLine n s jl = takeJ n jl +++ fromString s +++ dropJ (n + 1) jl
  numLines = getSize . snd . tag
  value = getScore . fst . tag

strToJLB :: String -> JLB
strToJLB = fromString

exJlb = foldr f Empty ["JoinListBuf", "line1", "line2"]
  where
    f a b = strToJLB a +++ b

main = runEditor editor exJlb
