-- Write a function to delete a node (except the tail) in a singly linked list, given only access to that node.

module DeleteNode
  (
    deleteNode
  , Link(..)
  ) where

deleteNode :: (Eq a) => a -> Link a -> Link a
deleteNode _ Empty = Empty
deleteNode a (b :-> c)
  | a == b = c
  | otherwise = a :-> (deleteNode a c)

infixr 6 :->
data Link a = Empty
            | a :-> Link a deriving (Eq, Show)
