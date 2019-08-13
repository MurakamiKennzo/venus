-- Given a linked list and a value x, partition it such that all nodes less than x come before nodes greater than or equal to x.

-- You should preserve the original relative order of the nodes in each of the two partitions.

module Partition
  (
    partition
  , fromList
  ) where

partition :: (Ord a) => Link a -> a -> Link a
partition link a = filterLink (< a) link <> filterLink (>= a) link

filterLink :: (a -> Bool) -> Link a -> Link a
filterLink f = foldr (\a b -> if f a then a :-> b else b) Empty

data Link a = Empty | a :-> Link a deriving (Show)

infixr 4 :->

fromList :: [a] -> Link a
fromList = foldr (:->) Empty

instance Foldable Link where
  foldMap f Empty = mempty
  foldMap f (a :-> link) = f a <> foldMap f link 

instance Semigroup (Link a) where
  Empty <> a = a
  (a :-> link) <> b = a :-> link <> b

instance Monoid (Link a) where
  mempty = Empty
