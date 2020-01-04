-- Given a singly linked list L: L0→L1→…→Ln-1→Ln,
-- reorder it to: L0→Ln→L1→Ln-1→L2→Ln-2→…

-- You may not modify the values in the list's nodes, only nodes itself may be changed.

module ReorderList
  (
    reorderList
  , Link(..)
  ) where

reorderList :: Link a -> Link a
reorderList = reorderList' . foldMap return

reorderList' :: [a] -> Link a
reorderList' [] = Empty
reorderList' [a] = a :-> Empty
reorderList' a = let b = head a
                     c = last a
                     d = init . tail $ a
                 in  b :-> c :-> reorderList' d

data Link a = Empty
            | a :-> Link a deriving (Eq, Show, Read)

infixr 3 :->

instance Foldable Link where
  foldMap _ Empty = mempty
  foldMap f (a :-> b) = f a <> foldMap f b
