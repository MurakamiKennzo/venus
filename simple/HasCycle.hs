-- Given a linked list, determine if it has a cycle in it.

-- To represent a cycle in the given linked list, we use an integer pos which represents the position (0-indexed) in the linked list where tail connects to. If pos is -1, then there is no cycle in the linked list.

module HasCycle
  (
    hasCycle
  , Link(..)
  ) where

hasCycle :: Link a -> Int -> Bool
hasCycle _ (-1) = False
hasCycle a b
  | c == 0 = False
  | b >= c = False
  | otherwise = True
  where c = length a

data Link a = Empty
            | Link a (Link a) deriving (Eq, Show)

instance Foldable Link where
  foldMap _ Empty = mempty
  foldMap f (Link a b) = f a <> foldMap f b
