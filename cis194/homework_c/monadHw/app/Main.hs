{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Control.Monad.Random
import Data.Bifunctor (bimap)
import GHC.Float (int2Double)

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield {attackers :: Army, defenders :: Army}

ttk :: Rand StdGen Bool
ttk = die >>= \a -> die >>= \b -> return (a > b)

attack :: Army -> Army -> Rand StdGen (Army, Army)
attack x y = (\win -> (x - cnt + win, y - win)) <$> winM
  where
    cnt = min x y
    winM = length . filter id <$> replicateM cnt ttk

battleAny :: Battlefield -> Army -> Army -> Rand StdGen Battlefield
battleAny (Battlefield m n) x y = fmap (uncurry Battlefield) $ bimap (+ (m - x)) (+ (n - y)) <$> attack x y

battle :: Battlefield -> Rand StdGen Battlefield
battle bat@(Battlefield m n) = battleAny bat x y
  where
    x = min (m - 1) 3
    y = min n 2

invade :: Battlefield -> Rand StdGen Battlefield
invade bat@(Battlefield 1 _) = return bat
invade bat@(Battlefield _ 0) = return bat
invade bat = battle bat >>= \bat' -> invade bat'

successProb :: Battlefield -> Rand StdGen Double
successProb = fmap sum . replicateM n . (fmap score . invade)
  where
    n = 1000
    score :: Battlefield -> Double
    score (Battlefield _ 0) = 1 / int2Double n
    score _ = 0

combineNum :: Int -> Int -> Integer
combineNum x' y' = product [y - x + 1 .. y] `div` product [1 .. x]
  where
    x = toInteger x'
    y = toInteger y'

exactSuccessProb :: Battlefield -> Double
exactSuccessProb (Battlefield x y)
  | x < 2 = 0
  | y < 1 = 1
  | otherwise = p ^ y * sum (map f [0 .. x - 2])
  where
    p = 5 / 12 :: Double
    q = 1 - p
    f :: Int -> Double
    f i = (q ^ i) * fromInteger (combineNum i (i + y - 1))

instance Show Battlefield where
  show bat = "attackers " ++ show (attackers bat) ++ ", defenders " ++ show (defenders bat)

main :: IO ()
main = do
  x <- getLine
  y <- getLine
  let bat = Battlefield (read x) (read y)
  print bat
  res <- evalRandIO $ successProb bat
  putStrLn $ "successProb : " ++ show res
  putStrLn $ "exactSuccessProb : " ++ show (exactSuccessProb bat)
