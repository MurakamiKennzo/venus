-- Given an array of non-negative integers, you are initially positioned at the first index of the array.

-- Each element in the array represents your maximum jump length at that position.

-- Your goal is to reach the last index in the minimum number of jumps.

module Jump
  (
    jump
  ) where

jump :: [Int] -> Int
jump [] = 0
jump [_] = 0
jump (x:xs) = let restXs = rest x xs in 
                if restXs == [] 
                  then maxStep
                  else minimum $ map ((1+) . jump) $ rest x xs

rest :: Int -> [Int] -> [[Int]]
rest x xs = map (flip drop xs) [0 .. x - 1]

maxStep = 100000
