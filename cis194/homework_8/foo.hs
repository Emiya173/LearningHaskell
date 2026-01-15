import Data.Bifunctor
import Data.List
import Data.Ord
import Data.Tree
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ f) (GL l tf) = GL (e : l) (f + tf)

instance Semigroup GuestList where
  (GL l lf) <> (GL r rf) = GL (l ++ r) (lf + rf)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun l@(GL _ fL) r@(GL _ fR)
  | fL < fR = r
  | otherwise = l

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f = go
  where
    go (Node v xs) = f v $ map go xs

nextLevel ::
  Employee ->
  [(GuestList, GuestList)] ->
  (GuestList, GuestList)
nextLevel e l = (e `glCons` f l', f l)
  where
    f = foldr (mappend . uncurry moreFun) mempty
    l' = map (first tailG) l
      where
        tailG (GL [] _) = mempty
        tailG (GL (e : es) f) = GL es (f - empFun e)

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

treeFold' :: (a -> b -> b) -> b -> Tree a -> b
treeFold' f e (Node v []) = f v e
treeFold' f e (Node v (x : xs)) = g e' (Node v xs)
  where
    g = treeFold' f
    e' = g e x

maxFun' :: Tree Employee -> GuestList
maxFun' = uncurry moreFun . f
  where
    f (Node v xs) = nextLevel v $ map f xs

format :: GuestList -> IO ()
format (GL l fun) = foldr (\x y -> y >> putStrLn x) (putStrLn total) l'
  where
    total = "Total fun:" ++ show fun
    l' = sortBy (comparing Data.Ord.Down) (map empName l)

main :: IO ()
main = readFile "company.txt" >>= (format . maxFun . read)
