-- You are given two non-empty linked lists representing two non-negative integers. The digits are stored in reverse order and each of their nodes contain a single digit. Add the two numbers and return it as a linked list.

-- You may assume the two numbers do not contain any leading zero, except the number 0 itself.

module AddTwoNumbers 
  (
    Link(..)
  , addTwoNumbers
  , fromList
  , toList
  ) where

data Link a = Empty | Node a (Link a) deriving (Show)

type Carry = Int

addTwoNumbers :: Link Int -> Link Int -> Link Int
addTwoNumbers = addTwoNumbers' 0

addTwoNumbers' :: Carry -> Link Int -> Link Int -> Link Int
addTwoNumbers' _ Empty Empty = Empty

addTwoNumbers' carry (Node a link) Empty = 
  let (carry', a') = add carry 0 a
  in  Node a' $ addTwoNumbers' carry' link Empty

addTwoNumbers' carry Empty (Node a link) = 
  let (carry', a') = add carry a 0
  in  Node a' $ addTwoNumbers' carry' Empty link

addTwoNumbers' carry (Node a1 link1) (Node a2 link2) =
  let (carry', a) = add carry a1 a2
  in  Node a $ addTwoNumbers' carry' link1 link2

add :: Carry -> Int -> Int -> (Int, Int)
add c a b = divMod (c + a + b) 10

fromList :: [a] -> Link a
fromList = foldr Node Empty . reverse

toList :: Link a -> [a]
toList Empty = []
toList (Node a link) = a : toList link
