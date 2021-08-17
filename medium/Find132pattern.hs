-- Given an array of n integers nums, a 132 pattern is a subsequence of three integers nums[i], nums[j] and nums[k] such that i < j < k and nums[i] < nums[k] < nums[j].

-- Return true if there is a 132 pattern in nums, otherwise, return false.

module Find132pattern
  (
    find132pattern
  ) where

find132pattern :: [Int] -> Bool
find132pattern [] = False
find132pattern (x:xs) = find132pattern' [(x, x)] xs

find132pattern' :: [(Int, Int)] -> [Int] -> Bool
find132pattern' _ [] = False
find132pattern' axs@((a, b):xs) (y:ys)
  | find132pattern'' axs y = True
  | y > b = find132pattern' ((a, y):xs) ys
  | y < a = find132pattern' ((y, y):axs) ys
  | otherwise = find132pattern' axs ys

find132pattern'' :: [(Int, Int)] -> Int -> Bool
find132pattern'' [] _ = False
find132pattern'' ((x, y):xs) z
  | x < z && y > z = True
  | otherwise = find132pattern'' xs z
