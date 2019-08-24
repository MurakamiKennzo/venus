-- Given an integer n, generate all structurally unique BST's (binary search trees) that store values 1 ... n.

module GenerateTrees
  (
    generateTrees
  ) where

generateTrees :: Int -> [Tree Int]
generateTrees n = generateTrees' [1 .. n]

generateTrees' :: [Int] -> [Tree Int]
generateTrees' [] = [Empty]
generateTrees' xs = do
  x <- xs
  let a = filter (< x) xs
      b = filter (> x) xs
  a' <- generateTrees' a
  b' <- generateTrees' b
  return $ Node x a' b'

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
