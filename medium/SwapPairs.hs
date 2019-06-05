-- Given a linked list, swap every two adjacent nodes and return its head.

-- You may not modify the values in the list's nodes, only nodes itself may be changed.

module SwapPairs
  (
    swapPairs
  , fromList
  ) where

infixr 5 :>:
data Link a = Empty | a :>: Link a deriving(Show, Eq)

fromList :: [a] -> Link a
fromList [] = Empty
fromList (x:xs) = x :>: fromList xs

swapPairs :: Link a -> Link a
swapPairs Empty = Empty
swapPairs l@(a :>: Empty) = l
swapPairs (a :>: b :>: link) = b :>: a :>: swapPairs link
