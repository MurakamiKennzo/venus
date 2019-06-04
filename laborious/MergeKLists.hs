-- Merge k sorted linked lists and return it as one sorted list. Analyze and describe its complexity.

module MergeKLists
  (
    mergeKLists,
    fromList,
  ) where

mergeKLists :: Ord a => [Link a] -> Link a
mergeKLists = foldl1 mergeTwoLists

mergeTwoLists :: Ord a => Link a -> Link a -> Link a
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
