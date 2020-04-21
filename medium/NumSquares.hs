-- Given a positive integer n, find the least number of perfect square numbers (for example, 1, 4, 9, 16, ...) which sum to n.

module NumSquares
  (
    numSquares
  ) where

import Data.List ( sortOn )

numSquares :: Int -> [Int]
numSquares a
  | a == 0 = []
  | otherwise = let b = takeWhile (<= a) squares
                in  head . sortOn length . map (\x -> x:numSquares (a - x)) $ b

squares :: [Int]
squares = map (^2) [1 ..]
