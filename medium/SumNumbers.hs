-- Given a binary tree containing digits from 0-9 only, each root-to-leaf path could represent a number.

-- An example is the root-to-leaf path 1->2->3 which represents the number 123.

-- Find the total sum of all root-to-leaf numbers.

-- Note: A leaf is a node with no children.

module SumNumbers
  (
    sumNumbers
  , Tree(..)
  ) where

import Data.Char (intToDigit)

sumNumbers :: Tree Int -> Int
sumNumbers = sum . fmap read . array . fmap intToDigit

array :: Tree Char -> [String]
array Empty = []
array (Node a left right) = if null d then [[a]] else map (a:) d
  where b = array left
        c = array right
        d = b <> c

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)
