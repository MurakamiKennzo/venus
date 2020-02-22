-- Reverse a singly linked list.

module ReverseList
  (
    reverseList
  , List(..)
  ) where

reverseList :: List a -> List a
reverseList Empty = Empty
reverseList (a :-> b) = reverseList b <> (a :-> Empty)

instance Semigroup (List a) where
  Empty <> a = a
  a <> Empty = Empty
  (a :-> c) <> b = a :-> c <> b

infixr 6 :->
data List a = Empty | a :-> List a deriving (Show)
