-- Given a string s containing an out-of-order English representation of digits 0-9, return the digits in ascending order.

module OriginalDigits
  (
    originalDigits
  ) where

import qualified Data.Map as Map
import Control.Monad ( join )

originalDigits :: String -> String
originalDigits xs = let map = originalDigitsMap xs
                    in  join $ originalDigits' map

originalDigitsMap :: String -> Map.Map Char Int
originalDigitsMap [] = mempty
originalDigitsMap (x:xs) = let map = originalDigitsMap xs
                           in  Map.insertWith (+) x 1 map

originalDigits' :: Map.Map Char Int -> [String]
originalDigits' map = let c0 = maybe 0 id $ map Map.!? 'z'
                          c2 = maybe 0 id $ map Map.!? 'w'
                          c4 = maybe 0 id $ map Map.!? 'u'
                          c6 = maybe 0 id $ map Map.!? 'x'
                          c8 = maybe 0 id $ map Map.!? 'g'
                          c3 = (maybe 0 id $ map Map.!? 'h') - c8
                          c5 = (maybe 0 id $ map Map.!? 'f') - c4
                          c7 = (maybe 0 id $ map Map.!? 's') - c6
                          c9 = (maybe 0 id $ map Map.!? 'i') - c5 - c6 - c8
                          c1 =  (maybe 0 id $ map Map.!? 'n') - c7 - c9 * 2
                      in  foldMap return [replicate c0 '0', replicate c1 '1', replicate c2 '2', replicate c3 '3', replicate c4 '4', replicate c5 '5', replicate c6 '6', replicate c7 '7', replicate c8 '9', replicate c9 '9']
