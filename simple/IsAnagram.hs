-- Given two strings s and t , write a function to determine if t is an anagram of s.

module IsAnagram
  (
    isAnagram
  ) where

import Data.List ( sort )

isAnagram :: String -> String -> Bool
isAnagram a b = sort a == sort b
