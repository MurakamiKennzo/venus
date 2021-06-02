-- Given an n-ary tree, return the level order traversal of its nodes' values.

module LevelOrder2
  (
    levelOrder
  ) where

levelOrder :: Tree a -> [[a]]
levelOrder EmptyNode = []
levelOrder (TreeNode a xs) = [a]:foldr (zip') [] (map levelOrder xs) 

zip' :: [[a]] -> [[a]] -> [[a]]
zip' [] xs = xs
zip' xs [] = xs
zip' (x:xs) (y:ys) = x <> y :zip' xs ys

data Tree a = EmptyNode
            | TreeNode a [Tree a] deriving (Show)
