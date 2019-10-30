-- Given a binary tree, determine if it is height-balanced.

-- For this problem, a height-balanced binary tree is defined as:

-- a binary tree in which the left and right subtrees of every node differ in height by no more than 1.

module IsBalanced 
  (
    isBalanced
  , Tree(..)
  ) where

isBalanced :: Tree a -> Bool
isBalanced Empty = True
isBalanced (Node _ a b) = let c = depth a
                              d = depth b
                          in  and [ abs (c - d) <= 1
                                  , isBalanced a
                                  , isBalanced b ]
  where depth :: Tree a -> Int
        depth Empty = 0
        depth (Node _ a b) = 1 + max (depth a) (depth b)

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show, Eq)
