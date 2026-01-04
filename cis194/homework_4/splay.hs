module Splay where

data SplayTree a = Empty | Node a (SplayTree a) (SplayTree a)
  deriving (Show, Eq)

splay :: (Ord a) => a -> SplayTree a -> SplayTree a
splay _ Empty = Empty
splay x t@(Node val l r)
  | x == val = t
  | x < val = case l of
      Empty -> t
      Node lval ll lr
        | x == lval -> Node lval ll (Node val lr r) -- Zig (右旋)
        | x < lval -> -- Zig-Zig (右右旋)
            let newLL = splay x ll
                newT = Node lval newLL (Node val lr r)
             in case newLL of
                  Empty -> newT
                  _ -> Node (valOf newLL) (leftOf newLL) (Node lval (rightOf newLL) (Node val lr r))
        | otherwise -> -- Zig-Zag (左右旋)
            let newLR = splay x lr
             in case newLR of
                  Empty -> Node val (Node lval ll Empty) r
                  _ -> Node (valOf newLR) (Node lval ll (leftOf newLR)) (Node val (rightOf newLR) r)
  | otherwise = case r of
      Empty -> t
      Node rval rl rr
        | x == rval -> Node rval (Node val l rl) rr -- Zag (左旋)
        | x > rval -> -- Zag-Zag (左左旋)
            let newRR = splay x rr
                newT = Node rval (Node val l rl) newRR
             in case newRR of
                  Empty -> newT
                  _ -> Node (valOf newRR) (Node rval (Node val l rl) (leftOf newRR)) (rightOf newRR)
        | otherwise -> -- Zag-Zig (右左旋)
            let newRL = splay x rl
             in case newRL of
                  Empty -> Node val l (Node rval Empty rr)
                  _ -> Node (valOf newRL) (Node val l (leftOf newRL)) (Node rval (rightOf newRL) rr)
  where
    valOf (Node v _ _) = v
    leftOf (Node _ l _) = l
    rightOf (Node _ _ r) = r

insert :: (Ord a) => a -> SplayTree a -> SplayTree a
insert x Empty = Node x Empty Empty
insert x t =
  let t'@(Node root l r) = splay x t
   in case compare x root of
        EQ -> t'
        LT -> Node x l (Node root Empty r)
        GT -> Node x (Node root l Empty) r

search :: (Ord a) => a -> SplayTree a -> (Bool, SplayTree a)
search x Empty = (False, Empty)
search x t =
  let t'@(Node root _ _) = splay x t
   in (root == x, t')

delete :: (Ord a) => a -> SplayTree a -> SplayTree a
delete _ Empty = Empty
delete x t =
  let t'@(Node root l r) = splay x t
   in if x /= root
        then t'
        else case l of
          Empty -> r
          _ ->
            let (Node maxL ll lr) = splay x l -- 实际上会提升左子树最大值
             in Node maxL ll r
