-- Given an integer n, return a string array answer (1-indexed) where:

-- answer[i] == "FizzBuzz" if i is divisible by 3 and 5.
-- answer[i] == "Fizz" if i is divisible by 3.
-- answer[i] == "Buzz" if i is divisible by 5.
-- answer[i] == i if non of the above conditions are true.

module FizzBuzz
  (
    fizzBuzz
  ) where

fizzBuzz :: Int -> [String]
fizzBuzz n = fizzBuzz' [1 .. n]

fizzBuzz' :: [Int] -> [String]
fizzBuzz' [] = []
fizzBuzz' (x:xs) = fizzBuzz'' x: fizzBuzz' xs
  where fizzBuzz'' :: Int -> String
        fizzBuzz'' n
          | n `mod` 15 == 0 = "FizzBuzz"
          | n `mod` 3 == 0 = "Fizz"
          | n `mod` 5 == 0 = "Buzz"
          | otherwise = show n
