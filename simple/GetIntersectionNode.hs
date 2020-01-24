-- Write a program to find the node at which the intersection of two singly linked lists begins.

module GetIntersectionNode
  (
    getIntersectionNode
  , Link(..)
  ) where

getIntersectionNode :: (Eq a) => Link a -> Link a -> Maybe a
getIntersectionNode a Empty = Nothing
getIntersectionNode a (b :-> bs) = if include b a then Just b else getIntersectionNode a bs

include :: (Eq a) => a -> Link a -> Bool
include a = foldr (\b c -> if a == b then True else c) False

data Link a = Empty 
            | a :-> Link a deriving (Eq, Show, Read)

infixr 6 :->

instance Foldable Link where
  foldMap f Empty = mempty
  foldMap f (a :-> as) = f a <> foldMap f as
