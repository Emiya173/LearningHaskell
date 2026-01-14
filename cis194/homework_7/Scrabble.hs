module Scrabble where

import Data.Char (toUpper)

newtype Score = Score Int deriving (Show)

instance Semigroup Score where
  Score x <> Score y = Score (x + y)

instance Monoid Score where
  mempty = Score 0

getScore (Score x) = x

score :: Char -> Score
score c = foldr f mempty l
  where
    u = toUpper c
    f (v, l) s
      | u `elem` l = s <> Score v
      | otherwise = s
    l = [(1, one), (2, two), (3, three), (4, four), (5, five), (8, eight), (10, ten)]
    one = ['A', 'E', 'I', 'O', 'U', 'L', 'N', 'R', 'S', 'T']
    two = ['D', 'G']
    three = ['B', 'C', 'M', 'P']
    four = ['F', 'H', 'V', 'W', 'Y']
    five = ['K']
    eight = ['J', 'X']
    ten = ['Q', 'Z']

scoreString :: String -> Score
scoreString = foldr (\c s -> score c <> s) mempty
