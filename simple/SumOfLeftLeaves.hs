-- Find the sum of all left leaves in a given binary tree.

module SumOfLeftLeaves
  (
    sumOfLeftLeaves
  ) where

sumOfLeftLeaves :: (Num a) => Tree a -> a
sumOfLeftLeaves Empty = 0
sumOfLeftLeaves (Node _ (Node a Empty Empty) right) = a + sumOfLeftLeaves right
sumOfLeftLeaves (Node _ left right) = sumOfLeftLeaves left + sumOfLeftLeaves right

data Tree a = Empty
            | Node a (Tree a) (Tree a)
