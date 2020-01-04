-- Given a binary tree, return the preorder traversal of its nodes' values.

module PreorderTraversal
  (
    preorderTraversal
  , Tree(..)
  ) where

preorderTraversal :: Tree a -> [a]
preorderTraversal Empty = []
preorderTraversal (Node a left right) = a : preorderTraversal left <> preorderTraversal right

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Eq, Show, Read)
