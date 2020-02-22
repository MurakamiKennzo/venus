-- Given two strings s and t, determine if they are isomorphic.

-- Two strings are isomorphic if the characters in s can be replaced to get t.

-- All occurrences of a character must be replaced with another character while preserving the order of characters. No two characters may map to the same character but a character may map to itself.

module IsIsomorphic
  (
    isIsomorphic
  ) where

import Prelude hiding ( lookup )
import Data.Map ( Map
                , empty
                , lookup
                , insert )

isIsomorphic :: String -> String -> Bool
isIsomorphic x y = let a = sign x
                       b = sign y
                   in  a == b

sign :: String -> String
sign = sign' 1 empty

sign' :: Int -> Map Char Int -> String -> String
sign' _ _ "" = ""
sign' n map (x:xs) = case lookup x map of
                      Just v -> show v ++ sign' n map xs
                      Nothing -> let n' = succ n in show n' ++ sign' n' (insert x n' map) xs
