-- Given a nested list of integers, implement an iterator to flatten it.

-- Each element is either an integer, or a list -- whose elements may also be integers or other lists.

module NestedIterator
  (
    nestedIterator
  , Interator(..)
  ) where

nestedIterator :: [Interator a] -> [a]
nestedIterator [] = []
nestedIterator (Value x:xs) = x: nestedIterator xs
nestedIterator (Interator xs':xs) = nestedIterator xs' <> nestedIterator xs

data Interator a = Value a
                 | Interator [Interator a]
