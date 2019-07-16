-- The set [1,2,3,...,n] contains a total of n! unique permutations.

-- Given n and k, return the kth permutation sequence.

module GetPermutation
  (
    getPermutation
  ) where

import Data.Char (intToDigit)
import Data.List (delete)

getPermutation :: Int -> Int -> String
getPermutation n k = map intToDigit . getPermutation' [1..n] $ k

getPermutation' :: [Int] -> Int -> [Int]
getPermutation' a 0 = reverse a
getPermutation' a 1 = a
getPermutation' xs k = let n = length xs
                           (i, k') = k `divMod` product [1 .. n - 1]
                           i' = if k' == 0 then i - 1 else i
                       in  xs !! i' : getPermutation' [xs !! index | index <- [0 .. n - 1], 
                                                                     index /= i'] k'
