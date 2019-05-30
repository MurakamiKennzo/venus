-- Given a string containing digits from 2-9 inclusive, return all possible letter combinations that the number could represent.

-- A mapping of digit to letters (just like on the telephone buttons) is given below. Note that 1 does not map to any letters.

module LetterCombinations
  (
    letterCombinations
  ) where

letterCombinations :: String -> [String]
letterCombinations = foldl1 list2 . map toString
  where list2 :: [String] -> [String] -> [String]
        list2 a b = (++) <$> a <*> b

toString :: Char -> [String]
toString '2' = ["a", "b", "c"]
toString '3' = ["d", "e", "f"]
toString '4' = ["g", "h", "i"]
toString '5' = ["j", "k", "l"]
toString '6' = ["m", "n", "o"]
toString '7' = ["p", "q", "r", "s"]
toString '8' = ["t", "u", "v"]
toString '9' = ["w", "x", "y", "z"]
