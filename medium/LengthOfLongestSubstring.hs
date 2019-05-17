-- Given a string, find the length of the longest substring without repeating characters.

module LengthOfLongestSubstring 
  (
    lengthOfLongestSubstring
  ) where

import Data.List (inits, nub)
import Data.Function (on)

lengthOfLongestSubstring :: String -> Int
lengthOfLongestSubstring = foldl max 0. map length . uniqStrings . subStrings

uniqStrings :: [String] -> [String]
uniqStrings = filter (\x -> ((==) `on` length) x $ nub x)

subStrings :: String -> [String]
subStrings [] = []
subStrings s @ (_:xs) = drop 1 (inits s) ++ subStrings xs
