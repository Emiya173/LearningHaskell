data SplayTree a = Empty | Node a (SplayTree a) (SplayTree a)
  deriving (Show)

-- 记录路径的上下文
-- GoLeft: 记录了父节点的值、右子树以及更高层的上下文
-- GoRight: 记录了父节点的值、左子树以及更高层的上下文
data Breadcrumb a
  = GoLeft a (SplayTree a)
  | GoRight a (SplayTree a)
  deriving (Show)

type Zipper a = (SplayTree a, [Breadcrumb a])

-- 向左下移动
goLeft :: Zipper a -> Zipper a
goLeft (Node v l r, bs) = (l, GoLeft v r : bs)

-- 向右下移动
goRight :: Zipper a -> Zipper a
goRight (Node v l r, bs) = (r, GoRight v l : bs)

-- 向上移动一层 (核心：在这里进行旋转重构)
goUp :: Zipper a -> Zipper a
goUp (t, GoLeft v r : bs) = (Node v t r, bs)
goUp (t, GoRight v l : bs) = (Node v l t, bs)

splayZipper :: (Ord a) => a -> Zipper a -> Zipper a
splayZipper x (t, bs)
  -- 1. 寻找目标：根据 BST 规则向下走
  | Node v l r <- t, x < v, not (isEmpty l) = splayZipper x (goLeft (t, bs))
  | Node v l r <- t, x > v, not (isEmpty r) = splayZipper x (goRight (t, bs))
  -- 2. 向上伸展：已经到达目标或叶子，开始成对回溯
  | otherwise = splayStep (t, bs)
  where
    isEmpty Empty = True
    isEmpty _ = False

-- 核心旋转逻辑：成对处理 Breadcrumbs
splayStep :: Zipper a -> Zipper a
splayStep (t, []) = (t, []) -- 已经在根部
splayStep (t, [b]) = rotate (t, [b]) -- 剩最后一步，做单旋 (Zig/Zag)
splayStep (t, b1 : b2 : bs) =
  case (b1, b2) of
    -- Zig-Zig (左左)
    (GoLeft g vG, GoLeft p vP) -> splayStep (Node g t (Node p vG vP), bs)
    -- Zag-Zag (右右)
    (GoRight g vG, GoRight p vP) -> splayStep (Node g (Node p vP vG) t, bs)
    -- Zig-Zag (左右)
    (GoRight g vG, GoLeft p vP) -> splayStep (Node g (Node p vP t) vG, bs)
    -- Zag-Zig (右左)
    (GoLeft g vG, GoRight p vP) -> splayStep (Node g vG (Node p t vP), bs)

-- 处理奇数高度的最后一次单旋
rotate :: Zipper a -> Zipper a
rotate (t, GoLeft v r : bs) = (Node v t r, bs) -- 实际上是逻辑上的提升
rotate (t, GoRight v l : bs) = (Node v l t, bs)

-- 查找：执行 splay 后取根节点
find :: (Ord a) => a -> SplayTree a -> (Maybe a, SplayTree a)
find x Empty = (Nothing, Empty)
find x t =
  let (t', _) = splayZipper x (t, [])
   in case t' of
        Node v _ _ | v == x -> (Just v, t')
        _ -> (Nothing, t')

-- 插入：在 Splay 后的基础上拆分
insert :: (Ord a) => a -> SplayTree a -> SplayTree a
insert x Empty = Node x Empty Empty
insert x t =
  let (Node v l r, _) = splayZipper x (t, [])
   in case compare x v of
        EQ -> Node v l r
        LT -> Node x l (Node v Empty r)
        GT -> Node x (Node v l Empty) r
