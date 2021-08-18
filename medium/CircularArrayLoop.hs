-- You are playing a game involving a circular array of non-zero integers nums. Each nums[i] denotes the number of indices forward/backward you must move if you are located at index i:

-- If nums[i] is positive, move nums[i] steps forward, and
-- If nums[i] is negative, move nums[i] steps backward.
-- Since the array is circular, you may assume that moving forward from the last element puts you on the first element, and moving backwards from the first element puts you on the last element.

-- A cycle in the array consists of a sequence of indices seq of length k where:

-- Following the movement rules above results in the repeating index sequence seq[0] -> seq[1] -> ... -> seq[k - 1] -> seq[0] -> ...
-- Every nums[seq[j]] is either all positive or all negative.
-- k > 1
-- Return true if there is a cycle in nums, or false otherwise.

module CircularArrayLoop
  (
    circularArrayLoop
  ) where

import Data.Sequence ( fromList
                     , Seq
                     , index
                     , update )
import Control.Monad.State ( State 
                           , evalState
                           , get
                           , put )

circularArrayLoop :: [Int] -> Bool
circularArrayLoop xs = evalState (circularArrayLoop' 0) (length xs, fromList xs)

c :: Int -> [Int] -> Int
c n xs = evalState (fastNext n) (length xs, fromList xs)

circularArrayLoop' :: Int -> State (Int, Seq Int) Bool
circularArrayLoop' n = do
  l <- fmap fst get
  if n >= l then pure False else circularArrayLoop'' (n, n) >>= \b -> if b then return True else setZero n >> circularArrayLoop' (succ n)

setZero :: Int -> State (Int, Seq Int) ()
setZero n = do
  n' <- slowNext n
  (l, seq) <- get
  let a = seq `index` n
      a' = seq `index` n'
  if a * a' > 0 then put (l, update n 0 seq) >> setZero n' else return ()

circularArrayLoop'' :: (Int, Int) -> State (Int, Seq Int) Bool
circularArrayLoop'' (slow, fast) = do
  slow' <- slowNext slow
  fast' <- fastNext fast
  slow'' <- slowNext slow'
  seq <- fmap snd get
  let a = seq `index` slow'
      a' = seq `index` slow''
  if a * a' > 0 then if slow' == fast' then pure $ slow' /= slow'' else circularArrayLoop'' (slow', fast') else pure False

slowNext :: Int -> State (Int, Seq Int) Int
slowNext n = do
  (l, seq) <- get
  return $ (n + seq `index` n + l) `mod` l

fastNext :: Int -> State (Int, Seq Int) Int
fastNext n = do
  n' <- slowNext n
  slowNext n'
