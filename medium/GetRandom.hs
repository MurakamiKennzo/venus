-- Given a singly linked list, return a random node's value from the linked list. Each node must have the same probability of being chosen.

-- Follow up:
-- What if the linked list is extremely large and its length is unknown to you? Could you solve this efficiently without using extra space?

module GetRandom 
  (
    getRandom
  ) where

import System.Random ( randomRIO )

getRandom :: Link a -> IO a
getRandom (a :-> link) = getRandom' 1 a link

getRandom' :: Int -> a -> Link a -> IO a
getRandom' n a Empty = return a
getRandom' n a (b :-> link) = do
  i <- randomRIO (0, n)
  let a' = if i == 0 then b else a
  getRandom' (succ n) a' link

infixr 5 :->
data Link a = Empty
            | a :-> Link a deriving (Show, Eq)
