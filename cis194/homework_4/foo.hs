import Data.List
import Data.Tree (drawTree)
import Data.Tree qualified

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate f
  where
    f x
      | even x = x `div` 2
      | otherwise = 3 * x + 1

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where
    insert x Leaf = Node 0 Leaf x Leaf
    insert x (Node h l v r)
      | height l > height r = balance $ Node h l v (insert x r)
      | otherwise = balance $ Node h (insert x l) v r

    balance (Node _ l v r) = Node h l v r
      where
        h = (+ 1) . maximum . map height $ [l, r]

    height Leaf = -1
    height (Node h _ _ _) = h

toDataTree :: (Show a) => Tree a -> Data.Tree.Tree String
toDataTree Leaf = Data.Tree.Node "L" []
toDataTree (Node i left val right) =
  Data.Tree.Node
    (show val ++ " [i=" ++ show i ++ "]")
    [toDataTree left, toDataTree right]

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x g acc -> g (f acc x)) id xs base

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2 : primes
  where
    primes = [2 * x + 1 | x <- ls \\ sieve]
    sieve = sort . filter (<= n) $ [x + y + 2 * x * y | (x, y) <- cartProd ls ls, x <= y]
    ls = [1 .. n]
