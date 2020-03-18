-- Given a 2D binary matrix filled with 0's and 1's, find the largest square containing only 1's and return its area.

module MaximalSquare
  (
    maximalSquare
  ) where

maximalSquare :: Matrix Int -> Int
maximalSquare [] = 0
maximalSquare ([]:_) = 0
maximalSquare [[a]] = a
maximalSquare a@(x:xs)
  | length a == length x && all (all (== 1)) a = length a ^ 2
  | otherwise = maximum [ maximalSquare $ tail a
                        , maximalSquare $ init a
                        , maximalSquare $ map tail a
                        , maximalSquare $ map init a ]

type Matrix a = [[a]]
