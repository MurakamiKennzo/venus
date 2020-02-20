-- Write an algorithm to determine if a number is "happy".

-- A happy number is a number defined by the following process: Starting with any positive integer, replace the number by the sum of the squares of its digits, and repeat the process until the number equals 1 (where it will stay), or it loops endlessly in a cycle which does not include 1. Those numbers for which this process ends in 1 are happy numbers.

module IsHappy
  (
    isHappy
  ) where

import Data.Map ( Map
                , empty
                , member
                , insert )

isHappy :: Int -> Bool
isHappy = isHappy' empty

isHappy' :: Map Int Bool -> Int -> Bool
isHappy' map n
  | n `member` map = False
  | otherwise = let m = happy n in if m == 1 then True else isHappy' (insert n True map) m
  where happy :: Int -> Int
        happy = foldr (\a b -> read [a] ^ 2 + b) 0 . show
