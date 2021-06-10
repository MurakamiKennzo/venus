-- Given the root of a binary tree and an integer targetSum, return the number of paths where the sum of the values along the path equals targetSum.

-- The path does not need to start or end at the root or a leaf, but it must go downwards (i.e., traveling only from parent nodes to child nodes).

module PathSum2
  (
    pathSum
  ) where

pathSum :: Tree Int -> Int -> Int
pathSum Empty n = if n == 0 then 1 else 0
pathSum (Node a left right) n
  | a == n = 1 + pathSum left n + pathSum right n
  | a < n = pathSum left n + pathSum right n
  | otherwise = pathSum left (a - n) + pathSum right (a - n) + pathSum left n + pathSum right n

data Tree a = Empty
            | Node a (Tree a) (Tree a)
