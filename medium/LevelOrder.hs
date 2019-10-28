-- Given a binary tree, return the level order traversal of its nodes' values. (ie, from left to right, level by level).

module LevelOrder
  (
    levelOrder
  , Tree(..)
  ) where

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
