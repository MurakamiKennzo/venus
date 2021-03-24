-- Given an array of integers with possible duplicates, randomly output the index of a given target number. You can assume that the given target number must exist in the array.

-- Note:
-- The array size can be very large. Solution that uses too much extra space will not pass the judge.

module Pick
  (
    pick
  ) where

import System.Random ( randomRIO )
import Control.Monad.Trans.Maybe ( MaybeT
                                 , runMaybeT )
import Control.Monad.Trans.Class ( lift )
import Control.Monad ( mzero )

pick :: [Int] -> Int -> IO (Maybe Int)
pick xs y = runMaybeT $ pick' mzero (0, 0) xs y

pick' :: MaybeT IO Int -> (Length, Index) -> [Int] -> Int -> MaybeT IO Int
pick' a _ [] _ = a
pick' a (l, i) (x:xs) y
  | x /= y = pick' a (l, succ i) xs y
  | otherwise = do
                  i' <- lift $ randomRIO (0, l)
                  let a' = if i' == 0 then return i else a
                  pick' a' (l + 1, succ i) xs y

type Length = Int

type Index = Int
