-- Given a triangle, find the minimum path sum from top to bottom. Each step you may move to adjacent numbers on the row below.

module MinimumTotal
  (
    minimumTotal
  ) where

import Data.List (elemIndices)

minimumTotal :: [[Int]] -> Int
minimumTotal [] = 0
minimumTotal [a] = minimum a
minimumTotal (x:xs) = let a = minimum x
                          b = elemIndices a x
                          c = map (perfectLine xs) b
                          d = map (perfectLine xs . succ) b
                      in  a + minimum (c ++ d)

perfectLine :: [[Int]] -> Int -> Int
perfectLine [] _ = 0
perfectLine [a] b = a !! b
perfectLine (x:xs) a = let b = x !! a
                           c = perfectLine xs a
                           d = perfectLine xs (a + 1)
                       in  b + min c d
