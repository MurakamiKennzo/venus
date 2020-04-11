-- Given a non-negative integer num, repeatedly add all its digits until the result has only one digit.

module AddDigits
  (
    addDigits
  ) where

addDigits :: Int -> Int
addDigits a
  | length b == 1 = a
  | otherwise = addDigits . read . foldr1 add' . map return $ b
  where b = show a

add' :: String -> String -> String
add' a b = show $ read a + read b
