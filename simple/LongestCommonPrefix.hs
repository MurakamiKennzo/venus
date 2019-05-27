-- Write a function to find the longest common prefix string amongst an array of strings.

-- If there is no common prefix, return an empty string "".

module LongestCommonPrefix
  (
    longestCommonPrefix
  ) where

longestCommonPrefix :: [String] -> String
longestCommonPrefix = foldl1 (+:)

infixl 6 +:
(+:) :: String -> String -> String
[] +: a = []
a +: [] = []
(x:xs) +: (y:ys) = if x == y then x : xs +: ys else []
