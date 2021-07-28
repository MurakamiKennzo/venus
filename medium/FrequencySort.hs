-- Given a string s, sort it in decreasing order based on the frequency of characters, and return the sorted string.

module FrequencySort
  (
    frequencySort
  ) where

import Data.Map ( insertWith
                , toList )
import Data.List ( sortOn )

frequencySort :: String -> String
frequencySort = toString . reverse . sortOn snd . toList . foldr (flip (insertWith (+)) 1) mempty
  where toString :: [(Char, Int)] -> String
        toString [] = []
        toString ((c, n):xs) = replicate n c <> toString xs
