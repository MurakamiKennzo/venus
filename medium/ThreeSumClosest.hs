-- Given an array nums of n integers and an integer target, find three integers in nums such that the sum is closest to target. Return the sum of the three integers. You may assume that each input would have exactly one solution.

module ThreeSumClosest
  (
    threeSumClosest
  ) where

import Data.List (sortOn, delete)

type Three =  (Int, Int, Int)

threeSumClosest :: [Int] -> Int -> Three
threeSumClosest a b = fst . head . sortOn ( abs . (subtract b) . snd) $ c
  where c = [add3 x y z | x <- a
                        , y <- delete x a
                        , z <- delete x . delete y $ a]

add3 :: Int -> Int -> Int -> (Three, Int)
add3 a b c = ((a, b, c), a + b + c)
