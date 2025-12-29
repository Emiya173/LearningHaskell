import Data.List

skips :: [a] -> [[a]]
skips x = [[y | (i, y) <- zip [1 ..] x, i `mod` n == 0] | n <- [1 .. length x]]

localMaxima :: [Integer] -> [Integer]
localMaxima (a : b : c : xs) = [b | b > a && b > c] ++ localMaxima (b : c : xs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = res ++ suffix
  where
    res = (unlines . reverse . filter f . transpose) [['*' | x <- xs, x == n] ++ space | n <- [0 .. 9]]
    f = not . all (== ' ')
    space = replicate (length xs) ' '
    suffix = "==========\n0123456789\n"
