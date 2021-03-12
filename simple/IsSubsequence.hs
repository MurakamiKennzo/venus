-- Given two strings s and t, check if s is a subsequence of t.

-- A subsequence of a string is a new string that is formed from the original string by deleting some (can be none) of the characters without disturbing the relative positions of the remaining characters. (i.e., "ace" is a subsequence of "abcde" while "aec" is not).

module IsSubsequence
  (
    isSubsequence
  ) where

isSubsequence :: String -> String -> Bool
isSubsequence "" "" = True
isSubsequence "" _ = True
isSubsequence _ "" = False
isSubsequence a@(x:xs) (y:ys)
  | x == y = isSubsequence xs ys
  | otherwise = isSubsequence a ys
