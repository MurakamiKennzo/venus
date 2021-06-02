-- You are given a doubly linked list which in addition to the next and previous pointers, it could have a child pointer, which may or may not point to a separate doubly linked list. These child lists may have one or more children of their own, and so on, to produce a multilevel data structure, as shown in the example below.

-- Flatten the list so that all the nodes appear in a single-level, doubly linked list. You are given the head of the first level of the list.

module Flatten
  (
    flatten
  ) where

flatten :: MultiLevelLink a -> [a]
flatten Empty = []
flatten (Node a next child) = a: flatten child <> flatten next

data MultiLevelLink a = Empty
                      | Node { value :: a
                             , next :: MultiLevelLink a
                             , child :: MultiLevelLink a }
