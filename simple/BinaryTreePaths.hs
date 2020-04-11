-- Given a binary tree, return all root-to-leaf paths.

-- Note: A leaf is a node with no children.

module BinaryTreePaths
  (
    binaryTreePaths
  ) where

binaryTreePaths :: (Show a) => Tree a -> [String]
binaryTreePaths Empty = []
binaryTreePaths (Node a Empty Empty) = [show a]
binaryTreePaths (Node a l r) = let b = binaryTreePaths l
                                   c = binaryTreePaths r
                               in  show a <-> b <> show a <-> c

infixl 7 <->
(<->) :: String -> [String] -> [String]
a <-> [] = []
a <-> xs = do
  x <- xs
  return $ a <> "->" <> x

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show, Eq)
