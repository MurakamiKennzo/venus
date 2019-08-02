-- Given two integers n and k, return all possible combinations of k numbers out of 1 ... n.

module Combine
  (
    combine
  ) where

combine :: Int -> Int -> [[Int]]
combine a b = combine' [1 .. a] b

combine' :: [Int] -> Int -> [[Int]]
combine' a 1 = map return a
combine' [] _ = []
combine' a@(x:xs) b = map (x:) (combine' xs (b - 1)) ++ combine' xs b
