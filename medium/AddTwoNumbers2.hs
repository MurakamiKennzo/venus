-- You are given two non-empty linked lists representing two non-negative integers. The most significant digit comes first and each of their nodes contains a single digit. Add the two numbers and return the sum as a linked list.

-- You may assume the two numbers do not contain any leading zero, except the number 0 itself.

module AddTwoNumbers2
  (
    addTwoNumbers
  ) where

addTwoNumbers :: Link Int -> Link Int -> Link Int
addTwoNumbers a b = addTwoNumbers' 0 a' b'
  where a' = buildReverseLink a
        b' = buildReverseLink b

addTwoNumbers' :: Int -> Link Int -> Link Int -> Link Int
addTwoNumbers' n Empty Empty = if n == 0 then Empty else 1 :-> Empty
addTwoNumbers' n (a :-> b) Empty = let (c, d) = (a + n) `divMod` 10
                                  in addTwoNumbers' c b Empty <> (d :-> Empty)
addTwoNumbers' n Empty a = addTwoNumbers' n a Empty
addTwoNumbers' n (a :-> b) (a' :-> b') = let (c, d) = (a + a' + n) `divMod` 10
                                         in  addTwoNumbers' c b b' <> (d :-> Empty)

buildReverseLink :: Link a -> Link a
buildReverseLink Empty = Empty
buildReverseLink (a :-> link) = buildReverseLink link <> (a :-> Empty)

data Link a = Empty
            | a :-> Link a deriving (Show)
infixr 5 :->

instance Semigroup (Link a) where
  a <> Empty = a
  Empty <> a = a
  (a :-> b) <> c = a :-> b <> c
