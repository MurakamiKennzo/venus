-- Given n, how many structurally unique BST's (binary search trees) that store values 1 ... n.

module NumTrees
  (
    numTrees
  ) where

numTrees :: Int -> Int
numTrees n = numTrees' [1 .. n]

numTrees' :: [Int] -> Int
numTrees' [] = 1
numTrees' [_] = 1
numTrees' xs = foldl (+) 0 $ do
  x <- xs
  let y = numTrees' . filter (< x) $ xs
      z = numTrees' . filter (> x) $ xs
  return $ y * z
