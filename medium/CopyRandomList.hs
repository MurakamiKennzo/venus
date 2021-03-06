-- A linked list is given such that each node contains an additional random pointer which could point to any node in the list or null.

-- Return a deep copy of the list.

module CopyRandomList
  (
    copyRandomList
  ) where

copyRandomList :: Link a -> Link a
copyRandomList = id

data Link a = Empty | Cons a (Link a) deriving (Eq, Show)
