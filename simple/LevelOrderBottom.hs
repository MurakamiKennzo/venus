-- Given a binary tree, return the bottom-up level order traversal of its nodes' values. (ie, from left to right, level by level from leaf to root).

module LevelOrderBottom
  (
    levelOrderBottom
  , Tree(..)
  ) where

levelOrderBottom :: Tree a -> [[a]]
levelOrderBottom = reverse . levelOrder

levelOrder :: Tree a -> [[a]]
levelOrder Empty = []
levelOrder (Node a left right) = let b = levelOrder left
                                     c = levelOrder right
                                     d = concat' b c
                                 in  [a] : d
  where concat' :: [[a]] -> [[a]] -> [[a]]
        concat' [] [] = []
        concat' (x:xs) [] = x : concat' xs []
        concat' [] (x:xs) = x : concat' [] xs
        concat' (x:xs) (y:ys) = (x ++ y) : concat' xs ys

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show, Eq)
