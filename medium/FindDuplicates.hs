-- Given an integer array nums of length n where all the integers of nums are in the range [1, n] and each integer appears once or twice, return an array of all the integers that appears twice.

-- You must write an algorithm that runs in O(n) time and uses only constant extra space.

module FindDuplicates
  (
    findDuplicates
  ) where

import Data.Sequence as Seq

findDuplicates :: [Int] -> [Int]
findDuplicates xs = findDuplicates' 0 $ Seq.fromList xs

findDuplicates' :: Int -> Seq.Seq Int -> [Int]
findDuplicates' a b = 
  case d of
    Nothing -> []
    Just g -> if g < 0 then c':e else f
  where c = b Seq.!? a >>= return . subtract 1 . abs
        c' = maybe 0 (+1) c
        d = c >>= (b Seq.!?)
        e = findDuplicates' (succ a) b
        f = findDuplicates' (succ a) $ maybe mempty id $ c >>= return . flip (Seq.adjust negate) b
