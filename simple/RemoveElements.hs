-- Remove all elements from a linked list of integers that have value val.

module RemoveElements
  (
    removeElements
  , Link(..)
  ) where

removeElements :: (Eq a) => a -> Link a -> Link a
removeElements _ Empty = Empty
removeElements a (b :-> c) = let d = removeElements a c 
                             in  if a == b then d else b :-> d

infixr 6 :->
data Link a = Empty | a :-> Link a deriving (Eq, Show)
