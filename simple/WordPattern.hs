-- Given a pattern and a string str, find if str follows the same pattern.

-- Here follow means a full match, such that there is a bijection between a letter in pattern and a non-empty word in str.

module WordPattern
  (
    wordPattern
  ) where

import Data.Map ( Map
                , (!?)
                , insert
                , empty )

wordPattern :: String -> String -> Bool
wordPattern a b = wordPattern' a == (wordPattern' . words $ b)

wordPattern' :: (Ord a) => [a] -> [Int]
wordPattern' = wordPattern'' 0 empty
  where wordPattern'' :: (Ord a) => Int -> Map a Int -> [a] -> [Int]
        wordPattern'' _ _ [] = []
        wordPattern'' n map (x:xs) = case map !? x of
                                        Just a -> a:wordPattern'' n map xs
                                        Nothing -> n:wordPattern'' (succ n) (insert x n map) xs
