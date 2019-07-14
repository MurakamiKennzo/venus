-- Given a positive integer n, generate a square matrix filled with elements from 1 to n2 in spiral order.

module GenerateMatrix
  (
    generateMatrix
  ) where

import Control.Applicative (ZipList(..) )

generateMatrix :: Int -> Matrix Int
generateMatrix n = let a = n ^ 2 in generateMatrix' [a, a - 1 .. 1] []

generateMatrix' :: (Eq a) => [a] -> Matrix a -> Matrix a
generateMatrix' [] a = a
generateMatrix' xs b
  | b == [] = if odd . length $ xs 
                then generateMatrix' (tail xs) [[head xs]]
                else let (a:b:c:d:e) = xs in generateMatrix' e [[d, c], [a, b]]
  | otherwise = let l = length b
                    n = l + 2
                    top = reverse . take n . drop (2 * l + n) $ xs
                    right = reverse . take l . drop (n + l) $ xs
                    bottom = take n . drop l $ xs
                    left = take l xs
                    level = getZipList $ (\x y -> [left !! y] ++ x ++ [right !! y]) <$> ZipList b <*> ZipList [0 .. l - 1]
                    leaveOut = drop (2 * (l + n)) xs
                    matrix = [top] ++ level ++ [bottom]
                in  generateMatrix' leaveOut matrix

type Matrix a = [[a]]
