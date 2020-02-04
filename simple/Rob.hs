-- You are a professional robber planning to rob houses along a street. Each house has a certain amount of money stashed, the only constraint stopping you from robbing each of them is that adjacent houses have security system connected and it will automatically contact the police if two adjacent houses were broken into on the same night.

-- Given a list of non-negative integers representing the amount of money of each house, determine the maximum amount of money you can rob tonight without alerting the police.

module Rob
  (
    rob
  ) where

rob :: [Money] -> Money
rob [] = 0
rob [a] = a
rob [a, b] = max a b
rob xs = let ys = init xs
             zs = init ys
             a = last xs
         in  max (rob zs + a) (rob ys)

type Money = Int
