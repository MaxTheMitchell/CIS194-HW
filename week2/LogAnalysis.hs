{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

module LogAnalysis where

import Log

parse :: String -> [LogMessage]
parse = map parseMessage . lines

parseMessage :: String -> LogMessage
parseMessage message = 
    case words message of 
        ("I":timestamp:str) -> LogMessage Info (read timestamp) (unwords str)
        ("W":timestamp:str) -> LogMessage Warning (read timestamp) (unwords str)
        ("E":errorcode:timestamp:str) -> LogMessage (Error $ read errorcode) (read timestamp) (unwords str)
        str -> Unknown (unwords str)

insert :: LogMessage -> MessageTree -> MessageTree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage@(LogMessage _ timestamp _) (Node leftBranch treeLog@(LogMessage _ treeTime _) rightBranch)
    | timestamp < treeTime = Node (insert logMessage leftBranch) treeLog rightBranch
    | otherwise = Node leftBranch  treeLog  (insert logMessage rightBranch)
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf 

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftBranch logMessage rightBranch) =
    inOrder leftBranch ++ [logMessage] ++ inOrder rightBranch

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = 
    map (\case
        (LogMessage _ _ str) -> str
        (Unknown str) -> str)
    . inOrder
    . build
    . filter (\case 
            (LogMessage (Error errorLevel) _ _) -> errorLevel >= 50
            _ -> False)   