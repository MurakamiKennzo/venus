-- Given n points on a 2D plane, find the maximum number of points that lie on the same straight line.

module MaxPoints
  (
    maxPoints
  ) where

import Prelude hiding (lines)
import Control.Monad ( filterM 
                     , guard )

maxPoints :: [Point] -> Int
maxPoints a
  | length a <= 1 = length a
  | otherwise = maximum . map length $ do
      b <- lines a
      return $ filter (flip inLinePoint b) a

inLinePoint :: Point -> Line -> Bool
inLinePoint (x, y) ((x1, y1), (x2, y2))
  | x1 == x2 = x == x1
  | y1 == y2 = y == y1
  | otherwise = (x - x1) / (x2 - x1) == (y - y1) / (y2 - y1)

lines :: [Point] -> [Line]
lines a = do
  b <- filterM (return [True, False]) a
  guard (length b == 2)
  return (b!!0, b!!1)

type Line = (Point, Point)

type Point = (Double, Double)
