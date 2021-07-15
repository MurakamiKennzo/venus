-- You are given n points in the plane that are all distinct, where points[i] = [xi, yi]. A boomerang is a tuple of points (i, j, k) such that the distance between i and j equals the distance between i and k (the order of the tuple matters).

-- Return the number of boomerangs.

module NumberOfBoomerangs 
  (
    numberOfBoomerangs
  ) where

import Control.Monad ( forM )
import qualified Data.Map as Map
import Control.Monad.State ( State
                           , state
                           , get
                           , put
                           , evalState )

numberOfBoomerangs :: (Num a, Ord a) => [Point a] -> Int
numberOfBoomerangs xs = flip evalState mempty $ do
  ys <- forM xs $ \a -> put mempty >> (forM xs $ \b -> setDistanceMap (a `pointDistanceNotPow` b)) >> get >>= return . calcNumberOfBoomerangs
  return $ sum ys

calcNumberOfBoomerangs ::  (Num a, Ord a) => Map.Map a Int -> Int
calcNumberOfBoomerangs m = sum $ (\x -> x * (x - 1)) `fmap` Map.elems m

setDistanceMap :: (Num a, Ord a) => a -> State (Map.Map a Int) ()
setDistanceMap a = state $ \s -> ((), Map.insertWith (+) a 1 s)

pointDistanceNotPow :: (Num a) => Point a -> Point a -> a
pointDistanceNotPow (x, y) (x', y') = (x - x') ^ 2 + (y - y') ^ 2

type Point a = (a, a)
