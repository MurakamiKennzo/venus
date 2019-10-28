-- Given a binary tree, find its maximum depth.

-- The maximum depth is the number of nodes along the longest path from the root node down to the farthest leaf node.

-- Note: A leaf is a node with no children.

module MaxDepth 
  (
    maxDepth
  , Tree(..)
  ) where

maxDepth :: Tree a -> Int
maxDepth Empty = 0
maxDepth (Node _ left right) = 1 + max (maxDepth left) (maxDepth right)

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show, Eq)
