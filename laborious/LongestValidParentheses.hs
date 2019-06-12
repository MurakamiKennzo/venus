-- Given a string containing just the characters '(' and ')', find the length of the longest valid (well-formed) parentheses substring.

module LongestValidParentheses
  (
    longestValidParentheses
  ) where

import Data.List (sort)

type Stack = [(Int, Char)]

longestValidParentheses :: String -> Int
longestValidParentheses = last . sort . stack [] [] . zip [0..]

stack :: [Int] -> Stack -> [(Int, Char)] -> [Int]
stack a _ [] = a
stack a [] (x:xs) = stack a [x] xs
stack a s@((y, yc):ys) (cur@(i, c):xs)
  | c == '(' = stack a (cur:s) xs
  | c == ')' && yc == ')' = stack a (cur:s) xs
  | otherwise =  
      let z = if ys == [] then -1 else fst . head $ ys
      in  stack (i - z :a) ys xs 
