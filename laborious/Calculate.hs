-- Implement a basic calculator to evaluate a simple expression string.

-- The expression string may contain open ( and closing parentheses ), the plus + or minus sign -, non-negative integers and empty spaces .

module Calculate
  (
    calculate
  ) where

calculate :: String -> Int
calculate = calculate' . filter (/= ' ')

calculate' :: String -> Int
calculate' ('(':xs) = calculate' . splitBracket 0 "" $ xs
calculate' str
  | '(' `notElem` str = let (a, b) = span (`elem` ['0' .. '9']) str
                            (a', b') = span (`elem` ['0' .. '9']) (tail str)
                        in  if a == ""
                              then calculate'' (head str:a') b'
                              else calculate'' a b
  | otherwise = let (a, _:b) = span (/= '(') str
                in  calculate' $ a <> (splitBracket 0 "" b)

calculate'' :: String -> String -> Int
calculate'' a "" = read a
calculate'' a (opt:b)
  | opt == '+' = calculate' $ show aa <> d
  | opt == '-' = calculate' $ show bb <> d
  where (c, d) = span (`elem` ['0' .. '9']) b
        aa = read a + read c
        bb = read a - read c

splitBracket :: Int -> String -> String -> String
splitBracket 0 bracket (')':xs) = (show . calculate' $ bracket) <> xs
splitBracket n bracket (')':xs) = splitBracket (pred n) (bracket <> [')']) xs
splitBracket n bracket ('(':xs) = splitBracket (succ n) (bracket <> ['(']) xs
splitBracket n bracket (x:xs) = splitBracket n (bracket <> [x]) xs
