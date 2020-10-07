-- Given an integer n, return 1 - n in lexicographical order.

-- For example, given 13, return: [1,10,11,12,13,2,3,4,5,6,7,8,9].

module LexicalOrder
  (
    lexicalOrder
  ) where

import Data.Function ( on )
import Data.List ( sortBy )

lexicalOrder :: Int -> [Int]
lexicalOrder n = sortBy (lexicalOrder' `on` show) $ [1 .. n]

lexicalOrder' :: String -> String -> Ordering
lexicalOrder' "" "" = EQ
lexicalOrder' a "" = GT
lexicalOrder' "" a = LT
lexicalOrder' (x:xs) (y:ys) = compare x y
                            <> lexicalOrder' xs ys
