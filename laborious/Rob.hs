-- You are a professional robber planning to rob houses along a street. Each house has a certain amount of money stashed. All houses at this place are arranged in a circle. That means the first house is the neighbor of the last one. Meanwhile, adjacent houses have security system connected and it will automatically contact the police if two adjacent houses were broken into on the same night.

-- Given a list of non-negative integers representing the amount of money of each house, determine the maximum amount of money you can rob tonight without alerting the police.

module Rob
  (
    rob
  ) where

rob :: [Int] -> Int
rob [] = 0
rob [x] = x
rob xs = let a = rob' $ init xs
             b = rob' $ tail xs
         in  max a b

rob' :: [Int] -> Int
rob' [] = 0
rob' [x] = x
rob' [x, y] = max x y
rob' (x:y:xs) = let a = x + rob' xs
                    b = rob' (y:xs)
                in  max a b
