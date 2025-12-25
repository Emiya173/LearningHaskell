import Data.List (minimumBy)
import Data.Ord (comparing)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x > 0 = x `mod` 10 : toDigitsRev (x `div` 10)
  | otherwise = []

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) (cycle [1, 2]) . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

type Peg = String

type Move = (Peg, Peg)

hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

-- 计算最优的 k 值（分割点）
-- 使用动态规划思想找到使总步数最少的 k
optimalK :: Int -> Int
optimalK n = fst $ minimumBy (comparing snd) candidates
  where
    candidates = [(k, totalMoves n k) | k <- [1 .. n - 1]]
    totalMoves n k = 2 * t4 k + t3 (n - k)
    -- 四柱汉诺塔的步数（递归）
    t4 1 = 1
    t4 m = minimum [2 * t4 i + t3 (m - i) | i <- [1 .. m - 1]]
    -- 三柱汉诺塔的步数
    t3 m = 2 ^ m - 1

hanoi4 :: Int -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 source target _ _ = [(source, target)]
hanoi4 n source target aux1 aux2 =
  let k = optimalK n
      -- 步骤1: 将前 k 个盘子移到 aux1（使用所有四个柱子）
      moves1 = hanoi4 k source aux1 aux2 target
      -- 步骤2: 将剩余 n-k 个盘子移到 target（只用三个柱子）
      moves2 = hanoi (n - k) source target aux2
      -- 步骤3: 将 k 个盘子从 aux1 移到 target（使用所有四个柱子）
      moves3 = hanoi4 k aux1 target aux2 source
   in moves1 ++ moves2 ++ moves3
