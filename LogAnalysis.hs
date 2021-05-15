{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import           Log

parseMessage :: String -> LogMessage

parseMessage s = case words s of
  ("E" : code : timestamp : message) ->
    LogMessage (Error (read code)) (read timestamp) (unwords message)
  ("I" : timestamp : message) ->
    LogMessage Info (read timestamp) (unwords message)
  ("W" : timestamp : message) ->
    LogMessage Warning (read timestamp) (unwords message)
  message -> Unknown (unwords message)

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message     Leaf = Node Leaf message Leaf
insert message@(LogMessage _ timestamp _) (Node l node@(LogMessage _ node_timestamp _) r)
  | timestamp < node_timestamp
  = Node (insert message l) node r
  | otherwise
  = Node l node (insert message r)
insert _ _ = error "Cannot have unknown node in a tree"

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder tree =
  let inOrder' Leaf         acc = acc
      inOrder' (Node l n r) acc = inOrder' l (n : (inOrder' r acc))
  in  inOrder' tree []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =
  let whatWentWrong' []       = []
      whatWentWrong' (x : xs) = case x of
        (LogMessage (Error code) _timestamp message) | code >= 50 ->
          message : (whatWentWrong' xs)
        _ -> whatWentWrong' xs
  in  whatWentWrong' . inOrder . build
