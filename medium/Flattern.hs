-- Given a binary tree, flatten it to a linked list.

module Flattern
  (
    flattern
  , Tree(..)
  ) where

flattern :: Tree a -> Link a
flattern Empty = EmptyLink
flattern (Node a b c) = a :-> flattern b +++ flattern c

data Link a = EmptyLink
            | a :-> (Link a) deriving (Eq, Show)

infixl 4 +++
(+++) :: Link a -> Link a -> Link a
EmptyLink +++ a = a
a +++ EmptyLink = a
(a :-> b) +++ c = a :-> (b +++ c)

data Tree a = Empty
            | Node a (Tree a) (Tree a) deriving (Eq, Show)

infixr 5 :->
