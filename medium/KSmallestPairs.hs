-- You are given two integer arrays nums1 and nums2 sorted in ascending order and an integer k.

-- Define a pair (u,v) which consists of one element from the first array and one element from the second array.

-- Find the k pairs (u1,v1),(u2,v2) ...(uk,vk) with the smallest sums.

module KSmallestPairs
  (
    kSmallestPairs
  ) where

import Data.Sequence ( Seq( (:|>) )
                     , (!?)
                     , index
                     , update
                     , singleton
                     , deleteAt
                     , drop )
import Data.Foldable ( toList )
import Data.Maybe ( fromJust )
import Prelude hiding ( max
                      , drop )

kSmallestPairs :: [Int] -> [Int] -> Int -> [(Int, Int)]
kSmallestPairs xs ys k = toList . fmap snd . drop 1 . capacity . kSmallestPairs' k $ [ (x + y, (x, y)) | x <- xs, y <- ys]

kSmallestPairs' :: (Ord a) => Int -> [a] -> GTHeap a
kSmallestPairs' n xs = foldr (\x h -> insert x h) heap xs
  where heap = GTHeap n 0 mempty

insert :: (Ord a) => a -> GTHeap a -> GTHeap a
insert a gtHeap@(GTHeap n c heap)
  | n <= 0 = GTHeap 0 0 mempty
  | c == 0 = gtHeap { current = c + 1
                    , capacity = singleton undefined :|> a }
  | c == n && pure a >= maxGTHeapItem = GTHeap n c heap
  | c == n && pure a < maxGTHeapItem = insert a (gtHeap { current = c - 1
                                                        , capacity = delete c heap })
  | otherwise = gtHeap { current = c + 1
                       , capacity = makeGTHeapify (c + 1) (heap :|> a) }
  where maxGTHeapItem = heap !? 1

delete :: (Ord a) => Int -> Seq a -> Seq a
delete n seq = delete' n 1 . deleteAt n . update 1 a $ seq
  where a = seq `index` n

delete' :: (Ord a) => Int -> Int -> Seq a -> Seq a
delete' n i seq
  | i >= n = seq
  | b < c && a < c = delete' n i'' . update i (fromJust c) . update i'' (fromJust a) $ seq
  | b > c && a < b = delete' n i' . update i (fromJust b) . update i' (fromJust a) $ seq
  | otherwise = seq
  where a = seq !? i
        i' = 2 * i
        i'' = 2 * i + 1
        b = seq !? i'
        c = seq !? i''

makeGTHeapify :: (Ord a) => Int -> Seq a -> Seq a
makeGTHeapify i seq
  | i <= 1 = seq
  | parent < a = makeGTHeapify i' $ update i parent . update i' a $ seq
  | otherwise = seq
  where i' = i `div` 2
        parent = seq `index` i'
        a = seq `index` i

data GTHeap a = GTHeap { max :: Int
                       , current :: Int
                       , capacity :: Seq a } deriving (Show, Eq)
