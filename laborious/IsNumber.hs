-- Validate if a given string can be interpreted as a decimal number.

module IsNumber
  (
    isNumber
  ) where

import Data.Char (isSpace)
import Data.List (dropWhile, dropWhileEnd)

isNumber :: String -> Bool
isNumber "" = False
isNumber s
  | x == '+' = isNumber' xs
  | x == '-' = isNumber' xs
  | otherwise = isNumber' a
  where a@(x:xs) = trim s


isNumber' :: String -> Bool
isNumber' = or . sequence [isNormal, isScientific]

isInteger :: String -> Bool
isInteger "" = False
isInteger s = isInteger' s
  where isInteger' :: String -> Bool
        isInteger' "" = True
        isInteger' (x:xs)
          | x >= '0' && x <= '9' = isInteger' xs
          | otherwise = False

isFloat :: String -> Bool
isFloat = and . sequence [('.' `elem`), isInteger . init' . dropWhileEnd (/= '.'), isInteger . tail' . dropWhile (/= '.')]

isScientific :: String -> Bool
isScientific = and . sequence [('e' `elem`), isNormal . init' . dropWhileEnd (/= 'e'), isIntegerScientific . tail' . dropWhile (/= 'e')]

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

isNormal :: String -> Bool
isNormal = or . sequence [isInteger, isFloat]

init' :: String -> String
init' "" = ""
init' a = init a

tail' :: String -> String
tail' "" = ""
tail' a = tail a

isIntegerScientific :: String -> Bool
isIntegerScientific "" = False
isIntegerScientific ('-':xs) = isInteger xs
isIntegerScientific s = isInteger s
