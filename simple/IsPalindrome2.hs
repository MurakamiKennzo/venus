-- Given a string, determine if it is a palindrome, considering only alphanumeric characters and ignoring cases.

-- Note: For the purpose of this problem, we define empty string as valid palindrome.

module IsPalindrome2
  (
    isPalindrome
  ) where

import Data.Char ( isAlphaNum 
                 , toLower )

isPalindrome :: String -> Bool
isPalindrome a = let b = map toLower . filter isAlphaNum $ a
                 in  reverse b == b
