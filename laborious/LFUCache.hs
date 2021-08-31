-- Design and implement a data structure for a Least Frequently Used (LFU) cache.

-- Implement the LFUCache class:

-- LFUCache(int capacity) Initializes the object with the capacity of the data structure.
-- int get(int key) Gets the value of the key if the key exists in the cache. Otherwise, returns -1.
-- void put(int key, int value) Update the value of the key if present, or inserts the key if not already present. When the cache reaches its capacity, it should invalidate and remove the least frequently used key before inserting a new item. For this problem, when there is a tie (i.e., two or more keys with the same frequency), the least recently used key would be invalidated.
-- To determine the least frequently used key, a use counter is maintained for each key in the cache. The key with the smallest use counter is the least frequently used key.

-- When a key is first inserted into the cache, its use counter is set to 1 (due to the put operation). The use counter for a key in the cache is incremented either a get or put operation is called on it.

module LFUCache
  (
    get
  , put
  , create
  ) where

import qualified Data.Map as Map
import qualified Control.Monad.State as State
import Control.Monad.Trans.Class

create :: (Eq k, Ord k, Monad m) => Int -> State.StateT (LFUCache k v) m ()
create n = State.put $ LFUCache n 0 mempty mempty

get :: (Eq k, Ord k, Monad m) => k -> State.StateT (LFUCache k v) m (Maybe v)
get k = State.StateT $ \lfucache -> return $ k `view` lfucache

put :: (Eq k, Ord k, Monad m) => (k, v) -> State.StateT (LFUCache k v) m ()
put (k, v) = State.StateT $ \lfucache -> return $ ((), (k, v) `push` lfucache)

view :: (Eq k, Ord k) => k -> LFUCache k v -> (Maybe v, LFUCache k v)
view k lfucache@(LFUCache capacity minFreq cachees freqes)
  | k `Map.member` cachees = let field = cachees Map.! k
                                 field' = field { freq = freq field + 1 }
                                 cachees' = Map.insert k field' cachees
                                 (minFreq', freqes') = updateFreqes minFreq (field, field') freqes
                             in  (Just $ value field, LFUCache capacity minFreq' cachees' freqes')
  | otherwise = (Nothing, lfucache)

push :: (Eq k, Ord k) => (k, v) -> LFUCache k v -> LFUCache k v
push (k, v) lfucache@(LFUCache capacity minFreq cachees freqes)
  | Map.size cachees < capacity = let field = LFUCacheField k v 1
                                      minFreq' = 1
                                      cachees' = Map.insert k field cachees
                                      freqes' = Map.insertWith (<>) 1 [field] freqes
                                  in  LFUCache capacity minFreq' cachees' freqes'
  | k `Map.member` cachees = let field = cachees Map.! k 
                                 field' = field { value = v 
                                                 , freq = freq field + 1 }
                                 cachees' = Map.insert k field' cachees
                                 (minFreq', freqes') = updateFreqes minFreq (field, field') freqes
                             in  LFUCache capacity minFreq' cachees' freqes'
  | otherwise = let lfucache' = deleteMinFreq lfucache
                in  push (k, v) lfucache'

deleteMinFreq :: (Eq k, Ord k) => LFUCache k v -> LFUCache k v
deleteMinFreq (LFUCache capacity minFreq cachees freqes) = let fields = freqes Map.! minFreq
                                                               field = last fields 
                                                               cachees' = Map.delete (key field) cachees
                                                               freqes' = Map.insert minFreq (init fields) freqes
                                                           in LFUCache capacity 0 cachees' freqes'

updateFreqes :: (Eq k, Ord k) => Int -> (LFUCacheField k v, LFUCacheField k v) -> Map.Map Int [LFUCacheField k v] -> (Int, Map.Map Int [LFUCacheField k v])
updateFreqes minFreq (field, field') freqes = let freq' = freq field
                                                  fields = filter (\f -> key f /= key field) $ freqes Map.! freq'
                                                  freqes' = Map.insertWith (<>) (freq field') [field'] . Map.insert freq' fields $ freqes
                                                  minFreq' = if length fields == 0 && minFreq == freq' then freq field' else minFreq
                                               in (minFreq', freqes')

data LFUCache k v = LFUCache { capacity :: Int
                             , minFreq :: Int
                             , cachees :: Map.Map k (LFUCacheField k v)
                             , freqes :: Map.Map Int [LFUCacheField k v] }

data LFUCacheField k v = LFUCacheField { key :: k
                                       , value :: v
                                       , freq :: Int } deriving (Eq)

instance Cache LFUCache where
  (<|) = view
  (|>) = push

class Cache c where
  (<|) :: (Eq k, Ord k) => k -> c k v -> (Maybe v, c k v)
  (|>) :: (Eq k, Ord k) => (k, v) -> c k v -> c k v
