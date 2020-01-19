-- Sort a linked list using constant space complexity.

module SortList
  (
    sortList
  , Link(..)
  ) where

import Data.List (sort)

sortList :: (Ord a) => Link a -> Link a
sortList = foldr Link Empty . sort . foldMap return

instance Foldable Link where
  foldMap _ Empty = mempty
  foldMap f (Link a b) = f a <> foldMap f b

data Link a = Empty
            | Link a (Link a) deriving (Eq, Show, Read)
