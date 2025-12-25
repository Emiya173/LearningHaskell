toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x > 0 = x `mod` 10 : toDigitsRev (x `div` 10)
  | otherwise = []

rev :: [a] -> [a]
rev [] = []
rev (x : xs) = rev xs ++ [x]

toDigits :: Integer -> [Integer]
toDigits x = rev $ toDigitsRev x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = rev $ doDouble 1 $ rev x

doDouble :: Integer -> [Integer] -> [Integer]
doDouble _ [] = []
doDouble num x = y * num : doDouble (num `mod` 2 + 1) ys
  where
    (y : ys) = x

sumDigits :: [Integer] -> Integer
sumDigits x = sum ys
  where
    ys = map (sum . toDigits) x

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0

type Peg = String

type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ (a, b) : hanoi (n - 1) c b a
