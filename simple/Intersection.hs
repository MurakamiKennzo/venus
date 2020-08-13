-- Given two arrays, write a function to compute their intersection.

module Intersection
  (
    intersection
  ) where

import Data.List ( nub )
import Control.Monad.Cont ( cont
                          , runCont )

intersection :: (Eq a) => [a] -> [a] -> [a]
intersection xs ys = (`runCont` id) $ do
  xs' <- cont $ \cont' -> cont' $ nub xs
  ys' <- cont $ \cont' -> cont' $ nub ys
  return [ x | x <- xs', x `elem` ys' ]
