-- A sequence of numbers is called a wiggle sequence if the differences between successive numbers strictly alternate between positive and negative. The first difference (if one exists) may be either positive or negative. A sequence with fewer than two elements is trivially a wiggle sequence.

-- For example, [1,7,4,9,2,5] is a wiggle sequence because the differences (6,-3,5,-7,3) are alternately positive and negative. In contrast, [1,4,7,2,5] and [1,7,4,5,5] are not wiggle sequences, the first because its first two differences are positive and the second because its last difference is zero.

-- Given a sequence of integers, return the length of the longest subsequence that is a wiggle sequence. A subsequence is obtained by deleting some number of elements (eventually, also zero) from the original sequence, leaving the remaining elements in their original order.

module WiggleMaxLength
  (
    wiggleMaxLength
  ) where

import Data.List ( maximumBy )
import Data.Function ( on )

wiggleMaxLength :: [Int] -> Int
wiggleMaxLength [] = 0
wiggleMaxLength xs = length . maximumBy (compare `on` length) . filter isWiggleList . foldr buildList [] $ xs
  where buildList :: Int -> [[Int]] -> [[Int]]
        buildList x [] = [[], [x]]
        buildList x xs = ((map (x:) xs) <> xs)

isWiggleList :: [Int] -> Bool
isWiggleList [] = True
isWiggleList [_] = True
isWiggleList (x:y:xs) = isWiggleList' wiggle y xs
  where wiggle = compare x y

isWiggleList' :: Ordering -> Int -> [Int] -> Bool
isWiggleList' _ _ [] = True
isWiggleList' order x (y:ys)
  | order == LT && order' == GT = isWiggleList' order' y ys
  | order == GT && order' == LT = isWiggleList' order' y ys
  | otherwise = False
  where order' = compare x y
