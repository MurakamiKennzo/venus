-- Design a data structure that supports all following operations in average O(1) time.

-- Note: Duplicate elements are allowed.
-- insert(val): Inserts an item val to the collection.
-- remove(val): Removes an item val from the collection if present.
-- getRandom: Returns a random element from current collection of elements. The probability of each element being returned is linearly related to the number of same value the collection contains.

module RandomizedCollection
  (
    insert
  , remove
  , getRandom
  ) where

import Control.Monad.State ( StateT(StateT) )
import System.Random
import Data.List ( delete )

insert :: (Eq a) => a -> StateT [a] IO ()
insert a = StateT $ \s -> return ((), a:s)

remove :: (Eq a) => a -> StateT [a] IO ()
remove a = StateT $ \s -> return ((), delete a s)

getRandom :: (Show a) => StateT [a] IO ()
getRandom = StateT $ \s -> randomItem s >>= return . (\item -> (item, s))
  where randomItem :: (Show a) => [a] -> IO ()
        randomItem xs = randomRIO (0, length xs - 1) >>= print . (xs !!)
