-- Given a set of candidate numbers (candidates) (without duplicates) and a target number (target), find all unique combinations in candidates where the candidate numbers sums to target.

-- The same repeated number may be chosen from candidates unlimited number of times.

-- Note:

-- All numbers (including target) will be positive integers.
-- The solution set must not contain duplicate combinations.

module CombinationSum
  (
    combinationSum
  ) where

import Data.List (nub, sort)

combinationSum :: [Int] -> Int -> [[Int]]
combinationSum [] _ = []
combinationSum [a] b = if a == b then [[a]] else []
combinationSum a@(x:xs) b = 
  let one = combinationSum xs b
      two = let c = b - x in if c < 0 then [] else if c == 0 then [[x]] else map (x:) $ combinationSum a c
  in   nub . map sort $ one ++ two
