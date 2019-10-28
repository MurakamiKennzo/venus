-- Two elements of a binary search tree (BST) are swapped by mistake.

-- Recover the tree without changing its structure.

module RecoverTree 
  (
    recoverTree
  , Tree(..)
  ) where

import Data.List (sort)

toList :: Tree a -> [a]
toList Empty = []
toList (Node a left right) = toList left ++ [a] ++ toList right

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show, Eq)

twoAlien :: (Eq a) => [a] -> [a] -> (a, a)
twoAlien (x:xs) (y:ys) = if x == y then twoAlien xs ys else (x, y)

recoverWithAlien :: (Eq a) => (a, a) -> Tree a -> Tree a
recoverWithAlien _ Empty = Empty
recoverWithAlien (a, b) (Node c left right)
  | a == c = Node b left' right'
  | b == c = Node a left' right'
  | otherwise = Node c left' right'
  where left' = recoverWithAlien (a, b) left
        right' = recoverWithAlien (a, b) right

recoverTree :: (Ord a) => Tree a -> Tree a          
recoverTree a = let b = toList a
                    c = twoAlien b (sort b)
                in  recoverWithAlien c a
