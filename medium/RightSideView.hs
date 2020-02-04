-- Given a binary tree, imagine yourself standing on the right side of it, return the values of the nodes you can see ordered from top to bottom.

module RightSideView
  (
    rightSideView
  , Tree(..)
  ) where

rightSideView :: Tree a -> [a]
rightSideView Empty = []
rightSideView (Node a l r) = a : right (rightSideView l) (rightSideView r)
  where right :: [a] -> [a] -> [a]
        right a [] = a
        right [] b = b
        right (_:xs) (y:ys) = y: right xs ys

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show, Eq, Read)
