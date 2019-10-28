-- Given preorder and inorder traversal of a tree, construct the binary tree.

-- Note:
-- You may assume that duplicates do not exist in the tree.

module BuildTree
  (
    buildTree
  , Tree(..)
  ) where

buildTree :: (Eq a) => [a] -> [a] -> Tree a
buildTree [] [] = Empty
buildTree (x:xs) a = let (b, c) = span' (/= x) a
                         d = filter (`elem` b) xs
                         e = filter (`elem` c) xs
                     in  Node x (buildTree d b) (buildTree e c)

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' f a = let (b, c) = span f a
            in  (b, if null c then [] else tail c)

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show, Eq)
