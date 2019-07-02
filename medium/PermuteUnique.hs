-- Given a collection of numbers that might contain duplicates, return all possible unique permutations.

module PermuteUnique
  (
    permuteUnique
  ) where

import Data.List ( delete
                 , nub )

permuteUnique :: [Int] -> [Permutation]
permuteUnique = nub. permute

permute :: [Int] -> [Permutation]
permute [] = []
permute [a] = [[a]]
permute a = concat [map (x:) $ permute y | x <- a, let y = delete x a]

type Permutation = [Int]
