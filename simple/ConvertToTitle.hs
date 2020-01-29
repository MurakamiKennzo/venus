-- Given a positive integer, return its corresponding column title as appear in an Excel sheet.

-- For example:
-- ```
--   1 -> A
--   2 -> B
--   3 -> C
--   ...
--   26 -> Z
--   27 -> AA
--   28 -> AB 
--   ...
-- ```

module ConvertToTitle
  (
    convertToTitle
  ) where

convertToTitle :: Int -> [Excel]
convertToTitle a
  | b == 0 = if c == 0 then [] else [d]
  | otherwise = convertToTitle (if c == 0 then pred b else b) <> [d]
  where (b, c) = a `divMod` 26
        d = toEnum $ if c == 0 then 25 else pred c

data Excel = A | B | C | D | E | F | G
           | H | I | J | K | L | M | N
           | O | P | Q | R | S | T | U
           | V | W | X | Y | Z deriving (Show, Eq, Ord, Enum, Bounded)
