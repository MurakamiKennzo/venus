Given a column title as appear in an Excel sheet, return its corresponding column number.

For example:
```
  A -> 1
  B -> 2
  C -> 3
  ...
  Z -> 26
  AA -> 27
  AB -> 28 
  ...
```

module TitleToNumber
  (
    titleToNumber
  ) where

titleToNumber :: [Excel] -> Int
titleToNumber (x:xs)
  | null xs = a
  | otherwise = a * 26 ^ length xs + titleToNumber xs
  where a = succ . fromEnum $ x

data Excel = A | B | C | D | E | F | G
           | H | I | J | K | L | M | N
           | O | P | Q | R | S | T | U
           | V | W | X | Y | Z deriving (Show, Eq, Ord, Enum, Bounded)
