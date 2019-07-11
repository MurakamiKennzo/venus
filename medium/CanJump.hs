-- Given an array of non-negative integers, you are initially positioned at the first index of the array.

-- Each element in the array represents your maximum jump length at that position.

-- Determine if you are able to reach the last index.

module CanJump
  (
    canJump
  ) where

canJump :: [Int] -> Bool
canJump [] = True
canJump [_] = True
canJump xxs@(x:xs)
  | x == 0 = False
  | otherwise = or [ canJump b | a <- [1 .. x]
                               , let b = drop a xxs ]
