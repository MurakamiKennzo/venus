-- Remove the minimum number of invalid parentheses in order to make the input string valid. Return all possible results.

-- Note: The input string may contain letters other than the parentheses ( and ).

module RemoveInvalidParentheses
  (
    removeInvalidParentheses
  ) where

import Control.Monad ( liftM2 )
import Data.Function ( on )
import Data.List ( sortOn
                 , groupBy
                 , nub )

removeInvalidParentheses :: String -> [String]
removeInvalidParentheses "" = [""]
removeInvalidParentheses xs = nub . last . groupBy ((==) `on` length) . sortOn length . filter (isValidParentheses 0) . foldr (liftM2 (++)) [""] $ [ ["", [x]] | x <- xs ]

isValidParentheses :: LeftParenthesesCount -> String -> Bool
isValidParentheses n "" = if n /= 0 then False else True
isValidParentheses 0 (')':_) = False
isValidParentheses n (x:xs)
  | x == '(' = isValidParentheses (succ n) xs
  | x == ')' = isValidParentheses (pred n) xs
  | otherwise = isValidParentheses n xs

type LeftParenthesesCount = Int
