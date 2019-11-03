-- Given a string S and a string T, count the number of distinct subsequences of S which equals T.

-- A subsequence of a string is a new string which is formed from the original string by deleting some (can be none) of the characters without disturbing the relative positions of the remaining characters. (ie, "ACE" is a subsequence of "ABCDE" while "AEC" is not).

module NumDistinct
  (
    numDistinct
  ) where

numDistinct :: String -> String -> Int
numDistinct _ "" = 1
numDistinct "" _ = 0
numDistinct a@(x:xs) b@(y:ys) = if x == y then c + d else d
  where c = numDistinct xs ys
        d = numDistinct xs b
