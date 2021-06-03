-- A gene string can be represented by an 8-character long string, with choices from 'A', 'C', 'G', and 'T'.

-- Suppose we need to investigate a mutation from a gene string start to a gene string end where one mutation is defined as one single character changed in the gene string.

-- For example, "AACCGGTT" --> "AACCGGTA" is one mutation.
-- There is also a gene bank bank that records all the valid gene mutations. A gene must be in bank to make it a valid gene string.

-- Given the two gene strings start and end and the gene bank bank, return the minimum number of mutations needed to mutate from start to end. If there is no such a mutation, return -1.

-- Note that the starting point is assumed to be valid, so it might not be included in the bank.

module MinMutation
  (
    minMutation
  ) where

import Data.List ( delete )

minMutation :: String -> String -> [String] -> Int
minMutation s e b = maybe (-1) id (minMutation' s e b) 

minMutation' :: String -> String -> [String] -> Maybe Int
minMutation' s e b
  | s == e = return 0
  | null a = Nothing
  | otherwise = minimum a
  where xs = findOneMutation s b
        a = filter (/= Nothing) . map (fmap (+1)) $ [ minMutation' x e (delete x b) | x <- xs]

findOneMutation :: String -> [String] -> [String]
findOneMutation x xs = filter (oneMutation x) xs
  where oneMutation :: String -> String -> Bool
        oneMutation = oneMutation' 0

        oneMutation' :: Int -> String -> String -> Bool
        oneMutation' n [] [] = if n == 1 then True else False
        oneMutation' _ [] _ = False
        oneMutation' _ _ [] = False
        oneMutation' n (x:xs) (y:ys)
          | x == y = oneMutation' n xs ys
          | otherwise = if n >= 1 then False else oneMutation' (succ n) xs ys
