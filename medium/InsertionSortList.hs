-- Sort a linked list using insertion sort.

module InsertionSortList
  (
    Link(..)
  , insertionSortList
  ) where

insertionSortList :: (Ord a) => Link a -> Link a
insertionSortList = insertionSortList' Empty

insertionSortList' :: (Ord a) => Link a -> Link a -> Link a
insertionSortList' a Empty = a
insertionSortList' a (Link x link) = insertionSortList' (insert x a) link
  where insert :: (Ord a) => a -> Link a -> Link a
        insert a Empty = Link a Empty
        insert a d@(Link b c)
          | a <= b = Link a d
          | otherwise = Link b (insert a c)

data Link a = Empty
            | Link a (Link a) deriving (Eq, Show, Read)
