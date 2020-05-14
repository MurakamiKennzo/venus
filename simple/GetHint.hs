-- You are playing the following Bulls and Cows game with your friend: You write down a number and ask your friend to guess what the number is. Each time your friend makes a guess, you provide a hint that indicates how many digits in said guess match your secret number exactly in both digit and position (called "bulls") and how many digits match the secret number but locate in the wrong position (called "cows"). Your friend will use successive guesses and hints to eventually derive the secret number.

-- Write a function to return a hint according to the secret number and friend's guess, use A to indicate the bulls and B to indicate the cows. 

-- Please note that both secret number and friend's guess may contain duplicate digits.

module GetHint 
  (
    getHint
  ) where

import Data.List ( sort
                 , group )

getHint :: String -> String -> String
getHint a b = let c = getHint' a b
                  d = getHint'' a b
              in  show c <> "A" <> show (d - c) <> "B"

getHint' :: String -> String -> Int
getHint' [] _ = 0
getHint' _ [] = 0
getHint' (x:xs) (y:ys) = (if x == y then 1 else 0) + getHint' xs ys

getHint'' :: String -> String -> Int
getHint'' a b = let c = fit (replicate 10 0) (group . sort $ a)
                    d = fit (replicate 10 0) (group . sort $ b)
                in  foldr (+) 0 $ zipWith min c d
  where fit :: [Int] -> [[Char]] -> [Int]
        fit a [] = a
        fit a ([]:xs) = fit a xs
        fit a (x@(y:_):xs) = let b = read [y] :: Int
                                 c = length x
                                 d = take b a <> [a !! b + c] <> drop (succ b) a
                             in  fit d xs
