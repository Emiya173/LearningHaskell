import Log

-- 解析单条日志消息
parseMessage :: String -> LogMessage
parseMessage str = case words str of
  ("I" : time : msg) -> parseNormal Info time msg
  ("W" : time : msg) -> parseNormal Warning time msg
  ("E" : level : time : msg) -> parseError level time msg
  _ -> Unknown str
  where
    parseNormal t time msg = case reads time of
      [(ts, "")] -> LogMessage t ts (unwords msg)
      _ -> Unknown str

    parseError level time msg = case (reads level, reads time) of
      ([(lv, "")], [(ts, "")]) -> LogMessage (Error lv) ts (unwords msg)
      _ -> Unknown str

-- 解析多行日志
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- 插入日志到二叉搜索树
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ x _) tree@(Node l m@(LogMessage _ y _) r)
  | x < y = Node (insert msg l) m r
  | otherwise = Node l m (insert msg r)

-- 构建二叉搜索树
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- 中序遍历(返回按时间排序的日志)
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

-- 提取严重错误消息
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extractMsg . filter isSevereError . inOrder . build
  where
    isSevereError (LogMessage (Error level) _ _) = level >= 50
    isSevereError _ = False

    extractMsg (LogMessage _ _ msg) = msg
    extractMsg _ = ""
