-- Given a string s, partition s such that every substring of the partition is a palindrome.

-- Return the minimum cuts needed for a palindrome partitioning of s.

module MinCut
  (
    minCut
  ) where

import Data.List ( sortOn )

minCut :: String -> Int
minCut "" = 0
minCut [_] = 0
minCut a = subtract 1 . length . head . sortOn length . filter (all isPalindrome) . splitString $ a

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
