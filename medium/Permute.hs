-- Given a collection of distinct integers, return all possible permutations.

module Permute
  (
    permute
  ) where

import Data.List (delete)

permute :: [Int] -> [Permutation]
permute [] = []
permute [a] = [[a]]
permute a = concat [map (x:) $ permute y | x <- a, let y = delete x a]

type Permutation = [Int]
