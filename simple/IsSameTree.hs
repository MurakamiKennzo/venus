-- Given two binary trees, write a function to check if they are the same or not.

-- Two binary trees are considered the same if they are structurally identical and the nodes have the same value.
  
  module IsSameTree 
  (
    isSameTree
  , Tree(..)
  ) where

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show, Eq)

isSameTree :: (Eq a) => Tree a -> Tree a -> Bool
isSameTree = (==)
