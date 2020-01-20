-- Given an input string, reverse the string word by word.

module ReverseWords
  (
    reverseWords
  ) where

reverseWords :: String -> String
reverseWords = unwords . reverse . words
