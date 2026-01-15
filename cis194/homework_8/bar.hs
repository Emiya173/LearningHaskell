import Data.List
import Data.Tree
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ f) (GL l tf) = GL (e : l) (f + tf)

instance Semigroup GuestList where
  (GL l lf) <> (GL r rf) = GL (l ++ r) (lf + rf)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node v subtrees) = f v (map (treeFold f) subtrees)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp res = (withEmp, withoutEmp)
  where
    withEmp = glCons emp $ mconcat (map snd res)
    withoutEmp = mconcat $ map (uncurry max) res

maxFun :: Tree Employee -> GuestList
maxFun t = uncurry max $ treeFold nextLevel t

format :: GuestList -> IO ()
format (GL lst fun) =
  putStrLn ("Total fun: " ++ show fun)
    >> (putStr . unlines . sort . map empName $ lst)

main :: IO ()
main = readFile "company.txt" >>= (format . maxFun . read)
