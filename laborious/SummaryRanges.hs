-- Given a data stream input of non-negative integers a1, a2, ..., an, ..., summarize the numbers seen so far as a list of disjoint intervals.

-- For example, suppose the integers from the data stream are 1, 3, 7, 2, 6, ..., then the summary will be:

-- [1, 1]
-- [1, 1], [3, 3]
-- [1, 1], [3, 3], [7, 7]
-- [1, 3], [7, 7]
-- [1, 3], [6, 7]
-- ...

module SummaryRanges
  (
    summaryRanges
  ) where

import Control.Monad.State ( StateT( StateT )
                           , lift
                           , runStateT )
import Data.List ( sort )

summaryRanges :: IO ((), NumRanges)
summaryRanges = runStateT summaryRanges' (NumRanges [])
  
summaryRanges' :: StateT NumRanges IO ()
summaryRanges' = lift getLine >>= return . read >>= addNum >> summaryRanges'

addNum :: Int -> StateT NumRanges IO ()
addNum x = addNum' x >>= \x -> lift (print x)

addNum' :: Int -> StateT NumRanges IO NumRanges
addNum' x = StateT $ putStateT
  where putStateT :: NumRanges -> IO (NumRanges, NumRanges)
        putStateT a@(NumRanges xs)
          | x `elem` xs = return (a, a)
          | otherwise = let a' = NumRanges $ sort (x:xs) in return (a', a')

newtype NumRanges = NumRanges { getNumRanges :: [Int] } deriving (Eq)

instance Show NumRanges where
  show xs = show . getRanges . getNumRanges $ xs

getRanges :: [Int] -> [(Int, Int)]
getRanges [] = []
getRanges [x] = [(x, x)]
getRanges (x:y:xs)
  | x + 1 == y = let (a, b) = getRanges' 1 x xs in a: getRanges b
  | otherwise = (x, x): getRanges (y:xs)

getRanges' :: Int -> Int -> [Int] -> ((Int, Int), [Int])
getRanges' n x [] = ((x, x + n), [])
getRanges' n x (y:ys)
  | x' + 1 == y = getRanges' (n + 1) x ys
  | otherwise = ((x, x'), y:ys)
  where x' = x + n
