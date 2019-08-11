-- Given n non-negative integers representing the histogram's bar height where the width of each bar is 1, find the area of largest rectangle in the histogram.

module LargestRectangleArea
  (
    largestRectangleArea
  ) where

largestRectangleArea :: [Int] -> Int
largestRectangleArea = largestRectangleArea' . zip [0 ..]

largestRectangleArea' :: [(Int, Int)] -> Int
largestRectangleArea' [] = 0
largestRectangleArea' (x:xs) = let a = largestRectangleArea'' x xs
                                   b = largestRectangleArea' xs
                               in  max a b

largestRectangleArea'' :: (Int, Int) -> [(Int, Int)] -> Int
largestRectangleArea'' (_, x) [] = x
largestRectangleArea'' (i, x) ((j, y):xs) = let a = min x y
                                                b = (j - i + 1) * a
                                                c = largestRectangleArea'' (i, a) xs
                                            in  max b c
