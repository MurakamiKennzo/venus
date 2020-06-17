-- Given a string which contains only lowercase letters, remove duplicate letters so that every letter appears once and only once. You must make sure your result is the smallest in lexicographical order among all possible results.

module RemoveDuplicateLetters
  (
    removeDuplicateLetters
  ) where

import Data.List ( sort )

removeDuplicateLetters :: String -> String
removeDuplicateLetters = head . sort . removeDuplicateLetters'

removeDuplicateLetters' :: String -> [String]
removeDuplicateLetters' [] = []
removeDuplicateLetters' [x] = [[x]]
removeDuplicateLetters' (x:xs)
  | x `elem` xs = map (x:) (removeDuplicateLetters' $ foldr (\a b -> if a == x then b else a:b) [] xs) 
                    <> removeDuplicateLetters' xs
  | otherwise = map (x:) (removeDuplicateLetters' xs)
