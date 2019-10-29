-- Given an array where elements are sorted in ascending order, convert it to a height balanced BST.

-- For this problem, a height-balanced binary tree is defined as a binary tree in which the depth of the two subtrees of every node never differ by more than 1.

module SortedArrayToBST
  (
    sortedArrayToBST
  ) where

sortedArrayToBST :: (Ord a) => [a] -> Tree a
sortedArrayToBST [] = Empty
sortedArrayToBST a = let (b, c, d) = divide a
                     in  Node b (sortedArrayToBST c) (sortedArrayToBST d)
  where divide :: [a] -> (a, [a], [a])
        divide a = let b = length a `div` 2
                       c = take b a
                       d = drop (b + 1) a
                       e = a !! b
                   in  (e, c, d)

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show, Eq)
