-- Given two arrays of length m and n with digits 0-9 representing two numbers. Create the maximum number of length k <= m + n from digits of the two. The relative order of the digits from the same array must be preserved. Return an array of the k digits.

module MaxNumber
  (
    maxNumber
  ) where

import Data.Function ( on )
import Data.List ( maximumBy )

maxNumber :: [Int] -> [Int] -> Int -> [Int]
maxNumber xs ys n = maximumBy (compare `on` readInt) . foldMap concat' $ [ (take' a xs, take' (n - a) ys) | a <- [0 .. n] ]
  where concat' :: ([[a]], [[a]]) -> [[a]]
        concat' ([], []) = []
        concat' (xs, ys) = concat $ (+++) <$> xs <*> ys

        readInt :: [Int] -> Int
        readInt [] = 0
        readInt (x:xs) = x * 10 ^ length xs + readInt xs

infix 4 +++
(+++) :: [a] -> [a] -> [[a]]
[] +++ [] = []
xs +++ [] = [xs]
[] +++ xs = [xs]
a@(x:xs) +++ b@(y:ys) = map (x:) (xs +++ b)
                          <> map (y:) (a +++ ys)

take' :: Int -> [a] -> [[a]]
take' _ [] = []
take' n a@(x:xs)
  | n <= 0 = []
  | n == 1 = map return a
  | otherwise = map (x:) (take' (pred n) xs)
                  <> take' n xs
