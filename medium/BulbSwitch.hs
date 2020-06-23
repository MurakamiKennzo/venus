module BulbSwitch
  (
    bulbSwitch
  ) where

-- There are n bulbs that are initially off. You first turn on all the bulbs. Then, you turn off every second bulb. On the third round, you toggle every third bulb (turning on if it's off or turning off if it's on). For the i-th round, you toggle every i bulb. For the n-th round, you only toggle the last bulb. Find how many bulbs are on after n rounds.

bulbSwitch :: Int -> Int
bulbSwitch n = length . filter (== Open) . foldl step (replicate n Close) $ [1 .. n]
  where step :: [Status] -> Int -> [Status]
        step [] _ = []
        step xs n = let (a, b) = splitAt n xs
                    in  (if length a == n then init a <> [switch . last $ a] else a) 
                          <> step b n
        
        switch :: Status -> Status
        switch Open = Close
        switch Close = Open

data Status = Open
            | Close deriving (Eq, Ord, Show, Read)
