-- Given a collection of candidate numbers (candidates) and a target number (target), find all unique combinations in candidates where the candidate numbers sums to target.

-- Each number in candidates may only be used once in the combination.

-- Note:

-- All numbers (including target) will be positive integers.
-- The solution set must not contain duplicate combinations.

module CombinationSum2
  (
    combinationSum2
  ) where

import Data.List (nub, sort)

combinationSum2 :: [Int] -> Int -> [[Int]]
combinationSum2 [] _ = []
combinationSum2 (x:xs) a =
  let b = combinationSum2 xs a
      c = let d = a - x in if d < 0 
                            then [] 
                            else if d == 0 
                            then [[x]] 
                            else map (x:) $ combinationSum2 xs d
  in  nub . map sort $ b ++ c
