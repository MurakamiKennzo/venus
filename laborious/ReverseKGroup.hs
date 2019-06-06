-- Given a linked list, reverse the nodes of a linked list k at a time and return its modified list.

-- k is a positive integer and is less than or equal to the length of the linked list. If the number of nodes is not a multiple of k then left-out nodes in the end should remain as it is.

module ReverseKGroup
  (
    reverseKGroup
  , fromList
  ) where

infixr 5 :>:
data Link a = Empty | a :>: Link a deriving(Show, Eq)

fromList :: [a] -> Link a
fromList [] = Empty
fromList (x:xs) = x :>: fromList xs

reverseKGroup :: Link a -> Int -> Link a
reverseKGroup link n = (foldl1 concat') . notLastReverse . (takeEvery n) $ link

takeEvery :: Int -> Link a -> [Link a]
takeEvery n link
  | l < n = [link]
  | otherwise = 
      let (link1, link2) = splitAt' n link
      in  link1 : takeEvery n link2
  where l = length' link

splitAt' :: Int -> Link a -> (Link a, Link a)
splitAt' n link = (take' n link, drop' n link)

take' :: Int -> Link a -> Link a
take' _ Empty = Empty
take' 0 _ = Empty
take' n (a :>: link) = a :>: take' (n - 1) link

drop' :: Int -> Link a -> Link a
drop' _ Empty = Empty
drop' 0 link = link
drop' n (_ :>: link) = drop' (n - 1) link

concat' :: Link a -> Link a -> Link a
concat' Empty link = link
concat' (a :>: link1) link2 = a :>: concat' link1 link2

reverse' :: Link a -> Link a
reverse' Empty = Empty
reverse' (a :>: link) = concat' (reverse' link) (a :>: Empty)

length' :: Link a -> Int
length' Empty = 0
length' (a :>: link) = 1 + length' link

notLastReverse :: [Link a] -> [Link a]
notLastReverse [] = []
notLastReverse a =
  let b = init a
      c = last a
  in  map reverse' b ++ [c]
