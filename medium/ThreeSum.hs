-- Given an array nums of n integers, are there elements a, b, c in nums such that a + b + c = 0? Find all unique triplets in the array which gives the sum of zero.

-- The solution set must not contain duplicate triplets.

module ThreeSum
  (
    threeSum
  ) where

import Data.List (delete, sort, nub)

threeSum :: [Int] -> [(Int, Int, Int)]
threeSum xs = nub [(x', y', z') | x <- xs
                                , let a = twoSum (-x) $ delete x xs
                                , a /= []
                                , (y, z) <- a
                                , let [x', y', z'] = sort [x, y, z]]

twoSum :: Int -> [Int] -> [(Int, Int)]
twoSum sum xs = [(x, y) | x <- xs
                        , y <- delete x xs
                        , x + y == sum]
