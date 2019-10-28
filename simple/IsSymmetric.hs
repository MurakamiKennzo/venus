-- Given a binary tree, check whether it is a mirror of itself (ie, symmetric around its center).

module IsSymmetric
  (
    isSymmetric
  , Tree(..)
  ) where

isSymmetric :: (Eq a) => Tree a -> Bool
isSymmetric Empty = True
isSymmetric a = a == swap a

swap :: Tree a -> Tree a
swap Empty = Empty
swap (Node a left right) = Node a (swap right) (swap left)

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show, Eq)
