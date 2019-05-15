-- Given an array of integers, return indices of the two numbers such that they add up to a specific target.

-- You may assume that each input would have exactly one solution, and you may not use the same element twice.

module TwoSum 
  ( 
    twoSum
  ) where

import Data.List (elemIndex, delete)

twoSum :: [Int] -> Int -> (Int, Int)
twoSum xs s = twoSum' xs [(x, y) | x <- xs, 
                                   y <- delete x xs, 
                                   x + y == s]

twoSum' :: [Int] -> [(Int, Int)] -> (Int, Int)
twoSum' _ [] = (-1, -1)
twoSum' xs ys = index xs . head $ ys

index :: (Eq a) => [a] -> (a, a) -> (Int, Int)
index xs (x, y) =
  let elemIndexXs = index' . flip elemIndex xs
      index' :: Maybe Int -> Int
      index' (Just a) = a
      index' Nothing = -1
  in  (elemIndexXs x, elemIndexXs y)
