-- Given an array of strings, group anagrams together.

module GroupAnagrams
  (
    groupAnagrams
  ) where

import Data.Function (on)
import Data.List ( sort 
                 , sortOn
                 , groupBy )

groupAnagrams :: [String] -> [[String]]
groupAnagrams = groupBy ((==) `on` sort) . sortOn sort
