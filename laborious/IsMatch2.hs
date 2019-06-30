-- Given an input string (s) and a pattern (p), implement wildcard pattern matching with support for '?' and '*'.

-- '?' Matches any single character.
-- '*' Matches any sequence of characters (including the empty sequence).
-- The matching should cover the entire input string (not partial).

-- Note:

-- s could be empty and contains only lowercase letters a-z.
-- p could be empty and contains only lowercase letters a-z, and characters like ? or *.

module IsMatch2
  (
    isMatch2
  ) where

isMatch2 :: String -> RegExp -> Bool
isMatch2 [] [] = True
isMatch2 _ [] = False
isMatch2 [] a = if all ('*'==) a then True else False
isMatch2 c@(a:as) d@(b:bs)
  | b == '?' = isMatch2 as bs
  | b == a = isMatch2 as bs
  | b == '*' = or [isMatch2 c bs, isMatch2 as d]
  | otherwise = False

type RegExp = String
