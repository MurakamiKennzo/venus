-- Implement a basic calculator to evaluate a simple expression string.

-- The expression string contains only non-negative integers, +, -, *, / operators and empty spaces . The integer division should truncate toward zero.

module Calculate 
  (
    calculate
  ) where

import Data.List ( dropWhileEnd
                 , isPrefixOf )

calculate :: String -> Int
calculate = calculate' . filter (/= ' ')

calculate' :: String -> Int
calculate' s
  | "-" `isPrefixOf` s = read s
  | multiAndDiv s = let (a, opt:b) = span multiAndDiv' s
                        (a', a'') = preOptNum a
                        (b', b'') = sufOptNum b
                    in  calculate' $ a' <> (show $ operator opt a'' b'') <> b'
  | addAndSub s = let (a, opt:b) = span addAndSub' s
                      (a', a'') = preOptNum a
                      (b', b'') = sufOptNum b
                  in  calculate' $ a' <> (show $ operator opt a'' b'') <> b'
  | otherwise = read s


multiAndDiv :: String -> Bool
multiAndDiv = do
  x <- ('*' `elem`)
  y <- ('/' `elem`)
  return $ x || y

multiAndDiv' :: Char -> Bool
multiAndDiv' = do
  x <- (/= '*')
  y <- (/= '/')
  return $ x && y

addAndSub :: String -> Bool
addAndSub = do
  x <- ('+' `elem`)
  y <- ('-' `elem`)
  return $ x || y

addAndSub' :: Char -> Bool
addAndSub' = do
  x <- (/= '+')
  y <- (/= '-')
  return $ x && y

preOptNum :: String -> (String, String)
preOptNum s = let a = dropWhileEnd (`elem` ['0' .. '9']) s
                  b = length a
              in  (a, drop b s)

sufOptNum :: String -> (String, String)
sufOptNum s = let (a, b) = span (`elem` ['0' .. '9']) s
              in  (b, a)

operator :: Char -> String -> String -> Int
operator opt a b
  | opt == '+' = a' + b'
  | opt == '-' = a' - b'
  | opt == '*' = a' * b'
  | opt == '/' = a' `quot` b'
  where a' = read a
        b' = read b
