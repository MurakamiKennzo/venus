module LRUCache
  (
    get
  , put
  , LRUCache(..)
  ) where

import Data.List ( find
                 , findIndex )
import Control.Monad.State ( State
                           , state )

get :: (Eq k) => k -> State (LRUCache k v) (Maybe v)
get k = state $ view k

put :: (k, v) -> State (LRUCache k v) ()
put a = state $ \s -> ((), push a s)

data LRUCache k v = LRUCache { capacity :: Int
                             , content :: [(k, v)] } deriving (Show, Eq)

view :: (Eq k) => k -> LRUCache k v -> (Maybe v, LRUCache k v)
view k (LRUCache n cache) = (fmap snd $ find ((== k) . fst) cache, LRUCache n $ k <|- cache)
  where (<|-) :: (Eq k) => k -> [(k, v)] -> [(k, v)]
        k' <|- xs = case findIndex ((== k') . fst) xs of
                      Nothing -> xs
                      Just i -> xs !! i : take i xs <> drop (succ i) xs

push :: (k, v) -> LRUCache k v -> LRUCache k v
push e (LRUCache n cache) = LRUCache n . take n . (e:) $ cache
