-- Given a binary tree, return the inorder traversal of its nodes' values.

module InorderTraversal
  (
    inorderTraversal
  , Tree(..)
  ) where

inorderTraversal :: Tree a -> [a]
inorderTraversal Empty = []
inorderTraversal (a :-> (left, right)) = inorderTraversal left <> [a] <> inorderTraversal right

infixr 4 :->

data Tree a = Empty | a :-> (Tree a, Tree a) deriving (Show)
