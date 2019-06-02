-- Given a linked list, remove the n-th node from the end of list and return its head.

module RemoveNthFromEnd
  (
    removeNthFromEnd,
    fromList,
  ) where

infixr 5 :>:
data Link a = Empty | a :>: Link a deriving(Show, Eq)

fromList :: [a] -> Link a
fromList [] = Empty
fromList (x:xs) = x :>: fromList xs

removeNthFromEnd :: Link a -> Int -> Link a
removeNthFromEnd link n = removeNthFromEnd' link (lengthLink link - n)

removeNthFromEnd' :: Link a -> Int -> Link a
removeNthFromEnd' Empty _ = Empty
removeNthFromEnd' (a :>: link) 0 = link
removeNthFromEnd' (a :>: link) n = a :>: removeNthFromEnd' link (n - 1)


lengthLink :: Link a -> Int
lengthLink Empty = 0
lengthLink (a :>: link) = 1 + lengthLink link
