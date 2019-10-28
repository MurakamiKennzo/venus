-- Given a binary tree, return the zigzag level order traversal of its nodes' values. (ie, from left to right, then right to left for the next level and alternate between).

module ZigZagLevelOrder
  (
    zigZagLevelOrder
  , Tree(..)
  ) where

import Prelude hiding ( Left 
                      , Right )

zigZagLevelOrder :: Tree a -> [[a]]
zigZagLevelOrder = zigZag Left . levelOrder

levelOrder :: Tree a -> [[a]]
levelOrder = levelOrder' []

levelOrder' :: [[a]] -> Tree a -> [[a]]
levelOrder' a Empty = a
levelOrder' a (Node b left right) = let c = levelOrder left
                                        d = levelOrder right
                                        e = concat' c d
                                    in  a ++ [[b]] ++ e
  where concat' :: [[a]] -> [[a]] -> [[a]]
        concat' [] [] = []
        concat' (x:xs) [] = x:concat' xs []
        concat' [] (x:xs) = x:concat' [] xs
        concat' (x:xs) (y:ys) = (x ++ y) : concat' xs ys

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show, Eq)

data ZigZag = Left | Right

zigZag :: ZigZag -> [[a]] -> [[a]]
zigZag _ [] = []
zigZag Left (x:xs) = x : zigZag Right xs
zigZag Right (x:xs) = reverse x : zigZag Left xs
