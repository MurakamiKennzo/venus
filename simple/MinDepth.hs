-- The minimum depth is the number of nodes along the shortest path from the root node down to the nearest leaf node.

-- Note: A leaf is a node with no children.

module MinDepth
  (
    minDepth
  , Tree(..)
  ) where

minDepth :: Tree a -> Int
minDepth Empty = 0
minDepth (Node _ a b) = 1 + min (minDepth a) (minDepth b)

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show, Eq)
