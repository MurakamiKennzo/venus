-- Given a string s, partition s such that every substring of the partition is a palindrome.

-- Return all possible palindrome partitioning of s.

module Partition2
  (
    partition
  ) where

partition :: String -> [[String]]
partition = filter (all isPalindrome) . splitString

splitString :: String -> [[String]]
splitString "" = []
splitString [a] = [[[a]]]
splitString (x:xs) = let a = splitString xs
                         b = map ([x]:) a
                         c = map (\y -> (x: head y) : tail y) a
                     in  b <> c

isPalindrome :: String -> Bool
isPalindrome a = a == reverse a
