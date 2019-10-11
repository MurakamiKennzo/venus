-- Given a binary tree, determine if it is a valid binary search tree (BST).

-- Assume a BST is defined as follows:

-- The left subtree of a node contains only nodes with keys less than the node's key.
-- The right subtree of a node contains only nodes with keys greater than the node's key.
-- Both the left and right subtrees must also be binary search trees.

module IsValidBST
  (
    isValidBST
  , BTree(..)
  ) where


isValidBST :: (Ord a) => BTree a -> Bool
isValidBST EmptyTree = True
isValidBST (BNode left a right) = and [ isValidBST' left a
                                      , isValidBST'' right a ]

isValidBST' :: (Ord a) => BTree a -> a -> Bool
isValidBST' EmptyTree _ = True
isValidBST' (BNode left a right) a' = and [ a < a'
                                          , isValidBST' left a
                                          , isValidBST'' right a ]

isValidBST'' :: (Ord a) => BTree a -> a -> Bool
isValidBST'' EmptyTree _ = True
isValidBST'' (BNode left a right) a' = and [ a > a'
                                           , isValidBST' left a
                                           , isValidBST'' right a ]

data BTree a = EmptyTree | BNode (BTree a) a (BTree a) deriving (Show, Read, Eq)
