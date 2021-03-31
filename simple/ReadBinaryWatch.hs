-- A binary watch has 4 LEDs on the top which represent the hours (0-11), and the 6 LEDs on the bottom represent the minutes (0-59).

-- Each LED represents a zero or one, with the least significant bit on the right.

-- Given a non-negative integer n which represents the number of LEDs that are currently on, return all possible times the watch could represent.

module ReadBinaryWatch
  (
    readBinaryWatch
  ) where

import Data.List ( delete
                 , permutations
                 , nub )

readBinaryWatch :: Int -> [String]
readBinaryWatch n = concat $ readBinaryWatch' xs
  where xs = [ (x, n - x) | x <- [0 .. n]]

readBinaryWatch' :: [Time] -> [[String]]
readBinaryWatch' xs = do
  (h, m) <- xs
  return $ readTime <$> hours h <*> minutes m

readTime :: Int -> Int -> String
readTime h m
  | m < 10 = show h <> ":0" <> show m 
  | otherwise = show h <> ":" <> show m

hours :: Int -> [Hour]
hours 0 = [0]
hours 1 = [1, 2, 4, 8]
hours n
  | n > 4 = []
  | otherwise = nub . filter (<= 11) . map (foldr (+) 0) . map (take n) $ xs
  where xs = permutations $ hours 1

minutes :: Int -> [Minute]
minutes 0 = [0]
minutes 1 = [1, 2, 4, 8, 16, 32]
minutes n
  | n > 6 = []
  | otherwise = nub . filter (<= 59) . map (foldr (+) 0) . map (take n) $ xs
  where xs = permutations $ minutes 1

type Time = (Hour, Minute)
type Hour = Int
type Minute = Int
