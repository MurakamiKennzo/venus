-- Given a 2D binary matrix filled with 0's and 1's, find the largest rectangle containing only 1's and return its area.

module MaximalRectangle
  (
    maximalRectangle
  ) where

maximalRectangle :: [[String]] -> Int
maximalRectangle = maximum . map largestRectangleArea . rectangle [] . map (map read)

rectangle :: [[Int]] -> [[Int]] -> [[Int]]
rectangle a [] = a
rectangle [] (x:xs) = rectangle [x] xs
rectangle x'@(x:xs) (y:ys) = rectangle (rectangle' x y : x') ys
  where rectangle' :: [Int] -> [Int] -> [Int]
        rectangle' [] [] = []
        rectangle' (x:xs) (y:ys) = (if y == 0 then 0 else 1 + x) : rectangle' xs ys

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
