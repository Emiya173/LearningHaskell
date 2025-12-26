module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage [] = Unknown ""
parseMessage (x : xs)
  | x == 'I' = parseNormal Info l
  | x == 'W' = parseNormal Warning l
  | x == 'E' = parseError l
  | otherwise = Unknown (x : xs)
  where
    l = words xs

parse :: String -> [LogMessage]
parse = map parseMessage . lines

parseNormal :: MessageType -> [String] -> LogMessage
parseNormal t (time : msg) = LogMessage t (read time) (unwords msg)

parseError :: [String] -> LogMessage
parseError (level : time : msg) = LogMessage (Error (read level)) (read time) (unwords msg)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg tree
  | x < y = Node (insert msg l) m r
  | otherwise = Node l m (insert msg r)
  where
    (LogMessage _ x _) = msg
    (LogMessage _ y _) = m
    (Node l m r) = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ m : inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extractMsg . inOrder . build . filter isError

isError :: LogMessage -> Bool
isError (LogMessage (Error level) _ _) = level >= 50
isError _ = False

extractMsg :: LogMessage -> String
extractMsg (LogMessage (Error _) _ msg) = msg
extractMsg _ = []
