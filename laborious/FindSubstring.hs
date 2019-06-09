-- You are given a string, s, and a list of words, words, that are all of the same length. Find all starting indices of substring(s) in s that is a concatenation of each word in words exactly once and without any intervening characters.

module FindSubstring
  (
    findSubstring
  ) where

import Data.List (permutations)

findSubstring :: String -> [String] -> [Int]
findSubstring s ws = findSubstring' s . map (foldl1 (++)) $ permutations ws

findSubstring' :: String -> [String] -> [Int]
findSubstring' [] _ = []
findSubstring' s [] = [0 .. length s - 1]
findSubstring' s w
  | h `elem` w = 0:indexes
  | otherwise = indexes
  where l = length . head $ w
        h = take l s
        indexes = map (+1) $ findSubstring' (drop 1 s) w
