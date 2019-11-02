-- Given a binary tree and a sum, determine if the tree has a root-to-leaf path such that adding up all the values along the path equals the given sum.

-- Note: A leaf is a node with no children.

module HasPathSum
  (
    hasPathSum
  , Tree(..)
  ) where

hasPathSum :: (Num a, Ord a) => Tree a -> a -> Bool
hasPathSum Empty 0 = True
hasPathSum Empty _ = False
hasPathSum (Node a b c) d
  | e < 0 = False
  | otherwise = or [ hasPathSum b e
                    , hasPathSum c e ]
  where e = d - a

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Eq, Show)
