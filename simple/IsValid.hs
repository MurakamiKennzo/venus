-- Given a string containing just the characters '(', ')', '{', '}', '[' and ']', determine if the input string is valid.

-- An input string is valid if:

-- Open brackets must be closed by the same type of brackets.
-- Open brackets must be closed in the correct order.

module IsValid
  (
    isValid
  ) where

isValid :: String -> Bool
isValid = (== 0) . length . foldl validChar ""
  where validChar :: String -> Char -> String
        validChar [] a = [a]
        validChar s@(x:xs) y = if dispel x y then xs else y:s

dispel :: Char -> Char -> Bool
dispel '(' ')' = True
dispel '[' ']' = True
dispel '{' '}' = True
dispel _ _ = False
