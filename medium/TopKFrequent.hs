-- Given a non-empty array of integers, return the k most frequent elements.

module TopKFrequent
  (
    topKFrequent
  ) where

import Data.List ( group
                 , sortOn
                 , sort )

topKFrequent :: [Int] -> Int -> [Int]
topKFrequent xs n = map head . take n . reverse . sortOn length . group . sort $ xs
