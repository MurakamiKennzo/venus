-- Given two words word1 and word2, find the minimum number of operations required to convert word1 to word2.

-- You have the following 3 operations permitted on a word:

-- Insert a character
-- Delete a character
-- Replace a character

module MinDistance
  (
    minDistance
  ) where

import Prelude hiding ( Word )

minDistance :: Word -> Word -> Int
minDistance "" "" = 0
minDistance (x:xs) "" = 1 + minDistance xs ""
minDistance "" (x:xs) = 1 + minDistance "" xs
minDistance (x:xs) (y:ys) = if x == y 
                              then minDistance xs ys 
                              else 1 + minimum [ minDistance (x:xs) ys
                                               , minDistance (xs) (y:ys)
                                               , minDistance xs ys ]

type Word = String
