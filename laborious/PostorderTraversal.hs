-- Given a binary tree, return the postorder traversal of its nodes' values.

module PostorderTraversal
  (
    postorderTraversal
  , Tree(..)
  ) where

postorderTraversal :: Tree a -> [a]
postorderTraversal Empty = []
postorderTraversal (Node a left right) = postorderTraversal left <> postorderTraversal right <> [a]

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Eq, Show, Read)