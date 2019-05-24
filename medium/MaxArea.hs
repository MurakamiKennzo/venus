-- Given n non-negative integers a1, a2, ..., an , where each represents a point at coordinate (i, ai). n vertical lines are drawn such that the two endpoints of line i is at (i, ai) and (i, 0). Find two lines, which together with x-axis forms a container, such that the container contains the most water.

-- Note: You may not slant the container and n is at least 2.

module MaxArea
  (
    maxArea
  ) where

maxArea :: [Int] -> Int
maxArea = foldl1 max . allArea

allArea :: [Int] -> [Int]
allArea [_] = []
allArea (x:xs) = foldl area [] xs ++ allArea xs
  where area :: [Int] -> Int -> [Int]
        area a b = let c = length a + 1 in min x b * c : a
