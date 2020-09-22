-- Implement the RandomizedSet class:

-- bool insert(int val) Inserts an item val into the set if not present. Returns true if the item was not present, false otherwise.
-- bool remove(int val) Removes an item val from the set if present. Returns true if the item was present, false otherwise.
-- int getRandom() Returns a random element from the current set of elements (it's guaranteed that at least one element exists when this method is called). Each element must have the same probability of being returned.
-- Follow up: Could you implement the functions of the class with each function works in average O(1) time?

module RandomizedSet
  (
    insert
  , remove
  , getRandom
  ) where

import Control.Monad.State ( StateT(StateT) )
import System.Random
import Data.List ( delete
                 , insert )

insert :: (Eq a) => a -> StateT [a] IO ()
insert a = StateT $ \s -> return ((), insert' a s)
  where insert' :: (Eq a) => a -> [a] -> [a]
        insert' x xs = if x `elem` xs then xs else x:xs

remove :: (Eq a) => a -> StateT [a] IO ()
remove a = StateT $ \s -> return ((), delete a s)

getRandom :: (Show a) => StateT [a] IO ()
getRandom = StateT $ \s -> randomItem s >>= return . (\item -> (item, s))
  where randomItem :: (Show a) => [a] -> IO ()
        randomItem xs = randomRIO (0, length xs - 1) >>= print . (xs !!)
