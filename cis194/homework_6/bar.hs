fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (drop 1 fibs2)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance (Show a) => Show (Stream a) where
  show = (++ ", more...") . show . take 7 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = x `Cons` streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = f x `Cons` streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = x `Cons` streamFromSeed f (f x)

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleave (Cons x xs) ys = Cons x (interleave ys xs)

ruler :: Stream Integer
ruler = interleave (streamRepeat 0) (streamMap (+ 1) ruler)

ruler2 :: Stream Integer
ruler2 = ruler' 0
  where
    ruler' n = interleave (streamRepeat n) (ruler' (n + 1))

x :: Stream Integer
x = 0 `Cons` (1 `Cons` streamRepeat 0)

instance Num (Stream Integer) where
  (+) (Cons x xs) (Cons y ys) = Cons (x + y) (xs + ys)
  (*) (Cons x xs) b@(Cons y ys) = Cons (x * y) (streamMap (* x) ys + xs * b)
  fromInteger = (`Cons` streamRepeat 0)
  negate = streamMap negate

instance Fractional (Stream Integer) where
  (/) a@(Cons x xs) b@(Cons y ys) = Cons (x `div` y) $ streamMap (`div` y) (xs - a / b * ys)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

type Matrix = (Integer, Integer, Integer, Integer)

instance Num Matrix where
  (*) (x1, x2, y1, y2) (m1, m2, n1, n2) = (x1 * m1 + y1 * m2, x1 * n1 + y1 * n2, x2 * m1 + y2 * m2, x2 * n1 + y2 * n2)

fib4 :: Integer -> Integer
fib4 n = let (_, _, _, res) = ((1, 1, 1, 0) :: Matrix) ^ n in res
