-- Invert a binary tree.

module InvertTree
  (
    invertTree
  , Tree(..)
  ) where

invertTree :: Tree a -> Tree a
invertTree Empty = Empty
invertTree (Node a l r) = Node a (invertTree r) (invertTree l)

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show, Eq)
