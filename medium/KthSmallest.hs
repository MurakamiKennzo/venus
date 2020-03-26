-- Given a binary search tree, write a function kthSmallest to find the kth smallest element in it.

-- Note: 
-- You may assume k is always valid, 1 ≤ k ≤ BST's total elements.

module KthSmallest
  (
    kthSmallest
  , Tree(..)
  ) where

kthSmallest :: (Ord a) => Tree a -> Int -> a
kthSmallest a n = (!! (pred n)) . foldMap return $ a

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Node a l r) = foldMap f l <> f a <> foldMap f r

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Show, Eq, Ord)
