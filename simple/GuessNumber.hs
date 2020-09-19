-- We are playing the Guess Game. The game is as follows:

-- I pick a number from 1 to n. You have to guess which number I picked.

-- Every time you guess wrong, I'll tell you whether the number is higher or lower.

-- You call a pre-defined API guess(int num) which returns 3 possible results (-1, 1, or 0):

-- -1 : My number is lower
--  1 : My number is higher
--  0 : Congrats! You got it!

module GuessNumber
  (
    guessNumber
  ) where

import System.Random ( randomRIO )

guessNumber :: Int -> IO ()
guessNumber n = do
  n' <- randomRIO (1, n)
  let guess = (`compare` n')
  print $ guessNumber' (1, n) guess

guessNumber' :: (Int, Int) -> (Int -> Ordering) -> Int
guessNumber' (a, b) guess
  | ord == LT = guessNumber' (n' + 1, b) guess
  | ord == EQ = n'
  | ord == GT = guessNumber' (a, n' - 1) guess
  where n' = (a + b) `div` 2
        ord = guess n'
