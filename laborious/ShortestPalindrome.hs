-- Given a string s, you are allowed to convert it to a palindrome by adding characters in front of it. Find and return the shortest palindrome you can find by performing this transformation.

module ShortestPalindrome
  (
    shortestPalindrome
  ) where

import Data.List ( delete
                 , elemIndices
                 , sortOn )

shortestPalindrome :: String -> String
shortestPalindrome s = let (a, b) = palindrome' s
                       in  reverse a <> s

palindrome' :: String -> (String, String)
palindrome' "" = ("", "")
palindrome' a = let b = maxPalindrome a
                in  (drop (length b) a, b)

maxPalindrome :: String -> String
maxPalindrome "" = ""
maxPalindrome (x:xs) = let indices = x `elemIndices` xs
                           y = map (flip splitAt xs) $ indices
                           a = sortOn length . map fst . filter (\(a, _) -> a == reverse a) $ y
                       in  if null indices then [x] else x:last a <> [x]
