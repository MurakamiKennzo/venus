-- Additive number is a string whose digits can form additive sequence.

-- A valid additive sequence should contain at least three numbers. Except for the first two numbers, each subsequent number in the sequence must be the sum of the preceding two.

-- Given a string containing only digits '0'-'9', write a function to determine if it's an additive number.

-- Note: Numbers in the additive sequence cannot have leading zeros, so sequence 1, 2, 03 or 1, 02, 3 is invalid.

module IsAdditiveNumber
  (
    isAdditiveNumber
  ) where

import Data.List ( isPrefixOf )
import Control.Monad ( guard )

isAdditiveNumber :: String -> Bool
isAdditiveNumber xs = any isAdditiveNumber' $ do
  (x, y) <- splitTwos xs
  (z, zs) <- splitTwos y
  guard (all (not . null) [x, z, zs])
  return (x, z, zs)

  where splitTwos :: String -> [(String, String)]
        splitTwos xs = [ splitAt n xs | n <- [0 .. length xs] ]

isAdditiveNumber' :: (String, String, String) -> Bool
isAdditiveNumber' (a, b, c)
  | c' `isPrefixOf` c = let (d, e) = splitAt (length c') c in if null e then True else isAdditiveNumber' (b, c', e)
  | otherwise = False
  where a' = read a
        b' = read b
        c' = show $ a' + b'
