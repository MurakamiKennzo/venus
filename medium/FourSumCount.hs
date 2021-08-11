-- Given four integer arrays nums1, nums2, nums3, and nums4 all of length n, return the number of tuples (i, j, k, l) such that:

--   0 <= i, j, k, l < n
--   nums1[i] + nums2[j] + nums3[k] + nums4[l] == 0

module FourSumCount
  (
    fourSumCount
  ) where

import Data.Map ( insertWith
                , member )
import Control.Monad ( guard )

fourSumCount :: [Int] -> [Int] -> [Int] -> [Int] -> Int
fourSumCount xs ys zs ms = length $ do
                              let a = foldr ((flip $ insertWith (+)) 1) mempty [ x + y | x <- xs, y <- ys ]
                              x <- zs
                              y <- ms
                              guard $ (negate $ x + y) `member` a
                              return ()
