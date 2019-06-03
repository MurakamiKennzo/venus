-- Merge two sorted linked lists and return it as a new list. The new list should be made by splicing together the nodes of the first two lists.

module MergeTwoLists
  (
    mergeTwoLists,
    fromList,
  ) where

mergeTwoLists :: (Ord a) => Link a -> Link a -> Link a
mergeTwoLists link Empty = link
mergeTwoLists Empty link = link
mergeTwoLists link1@(a :>: linka) link2@(b :>: linkb)
  | a <= b = a :>: mergeTwoLists linka link2
  | otherwise = b :>: mergeTwoLists link1 linkb

infixr 5 :>:
data Link a = Empty | a :>: Link a deriving(Show, Eq)

fromList :: [a] -> Link a
fromList [] = Empty
fromList (x:xs) = x :>: fromList xs
