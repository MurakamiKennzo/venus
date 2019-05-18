-- Given a string s, find the longest palindromic substring in s. You may assume that the maximum length of s is 1000.

module LongestPalindrome
  (
    longestPalindrome
  ) where

import Data.List (inits, sortOn)

longestPalindrome :: String -> String
longestPalindrome [] = []
longestPalindrome s = last . sortOn length . filter palindromeString . subStrings $ s

palindromeString :: String -> Bool
palindromeString s = s == reverse s

subStrings :: String -> [String]
subStrings [] = []
subStrings s @ (_:xs) = drop 1 (inits s) ++ subStrings xs
